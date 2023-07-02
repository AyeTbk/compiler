use std::collections::HashMap;

use crate::{
    instruction::{Instruction, Opcode, Operand},
    module::{Procedure, Variable},
    utils::Peephole,
};

pub fn stack_call_conv(proc: &mut Procedure) {
    // NOTE: Loads and Stores will be handled by another pass.

    // Allocate a stack slot for every proc parameter.
    // Only support virtual variables, panic on non virtual variables.
    // Replace every proc parameter with the allocated stack variable.
    let mut new_stack_vars = HashMap::new();
    for param in &mut proc.basic_blocks.entry.parameters {
        if !param.variable.is_virtual() {
            unimplemented!("non virtual variables not supported");
        }

        let param_stack_slot = proc
            .data
            .allocate_stack_slot_for(param.variable.to_non_stack().unwrap());
        let param_stack_var = Variable::Stack(param_stack_slot);
        new_stack_vars.insert(param.variable, param_stack_var);
        param.variable = param_stack_var;
    }

    // Modify every use of the proc parameters so they use the stack variable.
    Peephole::peep_instructions(proc, |_, _, instr| {
        for operand in instr.operands_mut() {
            if let Some(operand_var) = operand.to_variable() {
                if let Some(stack_var) = new_stack_vars.get(&operand_var) {
                    *operand = Operand::Var(*stack_var);
                }
            }
        }
    });
}

pub fn spill_all_virtual(proc: &mut Procedure) {
    // NOTE: Loads and Stores will be handled by another pass.

    // Entry block parameters are bound by their calling convention, so they will
    // not be spilled; they'll be mapped to None instead of Some(new stack var).
    let mut new_stack_vars: HashMap<Variable, Option<Variable>> = HashMap::new();

    // 1. Handle block parameters
    Peephole::peep_blocks(proc, |ph, i, block| {
        if i == 0 {
            for param in &mut block.parameters {
                if param.variable.is_virtual() {
                    new_stack_vars.insert(param.variable, None);
                }
            }
        } else {
            // allocate stack slot for all virtual parameters.
            // replace parameter with new stack variable.
            for param in &mut block.parameters {
                if param.variable.is_virtual() {
                    let stack_slot = ph
                        .data
                        .allocate_stack_slot_for(param.variable.to_non_stack().unwrap());
                    let stack_var = Variable::Stack(stack_slot);
                    new_stack_vars.insert(param.variable, Some(stack_var));
                    param.variable = stack_var;
                }
            }
        }
    });

    // 2. Handle instructions
    Peephole::peep_instructions(proc, |ph, _, instr| {
        match instr.opcode {
            Opcode::Load | Opcode::Store => return,
            _ => (),
        }

        // If dest is some and not stack:
        //  allocate a stack slot for it.
        //  replace it with the new stack variable.
        if let Some(dest_var) = instr.dst.as_mut() {
            if dest_var.is_virtual() {
                let stack_slot = ph
                    .data
                    .allocate_stack_slot_for(dest_var.to_non_stack().unwrap());
                let stack_var = Variable::Stack(stack_slot);
                new_stack_vars.insert(*dest_var, Some(stack_var));
                *dest_var = stack_var;
            }
        }

        // For every operand:
        //  replace with corresponding previously allocated stack variable.
        for operand in instr.operands_mut() {
            if let Some(operand_var) = operand.to_variable() {
                if !operand_var.is_virtual() {
                    continue;
                }
                if let Some(stack_var) = new_stack_vars.get(&operand_var).unwrap() {
                    *operand = Operand::Var(*stack_var);
                }
            }
        }

        // Handle condition operands
        if let Some(cond) = &mut instr.cond {
            for operand in cond.operands_mut() {
                if let Some(operand_var) = operand.to_variable() {
                    if operand_var.is_virtual() {
                        if let Some(stack_var) = new_stack_vars.get(&operand_var).unwrap() {
                            *operand = Operand::Var(*stack_var);
                        }
                    }
                }
            }
        }
    });
}

pub fn generate_loads_stores(proc: &mut Procedure) {
    // For every instruction:
    //   if instr is a jump to another block:
    //     for every operand:
    //       - panic if not a stack operand (handle that case later).
    //       - load from the stack operand to a new virtual variable.
    //       - make sure the associated target block parameter is a stack var (else panic! handle that later).
    //       - store the new virtual variable to the associated target block parameter.
    //       - replace the operand with the asssociated target block parameter.
    //   else:
    //     - insert loads to new virtual variables before every use of a given stack operand,
    //        and replace said operand with new virtual variable.
    //     - replace present dst with new virtual variable and insert stores for them after the
    //        instruction. Remember what virtual var maps to what stack var for later substitutions.
    //   - handle condition operands like non jump instr operand above.
    let mut block_parameters = HashMap::new();
    for block in proc.basic_blocks.iter() {
        block_parameters.insert(block.name.clone(), block.parameters.clone());
    }

    Peephole::peep_instructions(proc, |ph, i, instr| {
        // Handle source operands
        if matches!(instr.opcode, Opcode::Jump) {
            let target_name = instr.target_block.as_ref().unwrap();
            let target_paramters = block_parameters.get(target_name).unwrap();
            assert_eq!(instr.operands().count(), target_paramters.len());
            for (operand, param) in instr.operands_mut().zip(target_paramters) {
                let operand_stack_slot = operand
                    .to_variable()
                    .and_then(|v| v.as_stack())
                    .expect("non stack operand not currently supported");
                let param_stack_slot = param
                    .variable
                    .as_stack()
                    .expect("non stack block parameter not currently supported");
                let new_virtual_var = ph.data.acquire_new_virtual_variable();
                ph.insert_before(i, Instruction::load(new_virtual_var, operand_stack_slot));
                ph.insert_before(i, Instruction::store(param_stack_slot, new_virtual_var));
                *operand = Operand::Var(param.variable);
            }
        } else {
            for operand in instr.operands_mut() {
                if let Some(operand_var) = operand.to_variable() {
                    if let Some(stack_slot) = operand_var.as_stack() {
                        let new_virtual_var = ph.data.acquire_new_virtual_variable();
                        *operand = Operand::Var(new_virtual_var);
                        ph.insert_before(i, Instruction::load(new_virtual_var, stack_slot));
                    }
                }
            }
        }

        // Handle destination
        if let Some(dst_var) = &mut instr.dst {
            if let Some(stack_slot) = dst_var.as_stack() {
                let new_virtual_var = ph.data.acquire_new_virtual_variable();
                *dst_var = new_virtual_var;
                ph.insert_after(i, Instruction::store(stack_slot, new_virtual_var));
            }
        }

        // Handle condition operands
        if let Some(cond) = &mut instr.cond {
            for operand in cond.operands_mut() {
                if let Some(operand_var) = operand.to_variable() {
                    if let Some(stack_slot) = operand_var.as_stack() {
                        let new_virtual_var = ph.data.acquire_new_virtual_variable();
                        *operand = Operand::Var(new_virtual_var);
                        ph.insert_before(i, Instruction::load(new_virtual_var, stack_slot));
                    }
                }
            }
        }
    });
}
