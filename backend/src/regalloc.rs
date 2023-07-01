use crate::{
    instruction::{Instruction, Opcode},
    module::{Procedure, Variable},
    utils::Peephole,
};

pub enum InstructionConstraint {
    FirstOperandIsDestination,
}

// Spill everything; that is, allocate everything on the stack.
// The idea is: everytime a variable needs to be read, load it from its stack slot just
// before use, and when a variable is written to, store it to its stack slot just after.
// Block parameters need to be stored in their own stack slots aswell, so that following
// instructions can safely blindly load them if needed.
// Note: this is intended as a "generic" allocation scheme, the result still needs to go
// through a proper register allocation pass for the intended output architecture.
// FIXME SSA is not being respected right now
pub fn spillalloc(proc: &mut Procedure) {
    // 1. Handle block parameters
    Peephole::peep_blocks(proc, |ph, _, block| {
        //  allocate stack slot for all non-stack parameters.
        //  insert store(stack slot, parameter) for all non-stack parameters.
        for param in &block.parameters {
            if let Some(non_stack_param_variable) = param.variable.to_non_stack() {
                let stack_slot = ph.data.allocate_stack_slot_for(non_stack_param_variable);
                ph.insert_before(0, Instruction::store(stack_slot, param.variable));
            }
        }
    });

    // 2. Handle instructions
    Peephole::peep_instructions(proc, |ph, i, instr| {
        match instr.opcode {
            Opcode::Load | Opcode::Store => return,
            _ => (),
        }

        // If dest is some and not stack:
        //  allocate a stack slot for it
        //  insert store(stack slot, dest) after instruction.
        if let Some(dest) = instr.dst {
            if let Some(dest_non_stack) = dest.to_non_stack() {
                let stack_slot = ph.data.allocate_stack_slot_for(dest_non_stack);
                ph.insert_after(i, Instruction::store(stack_slot, dest));
            }
        }

        // For every operand:
        //  Dont do anything on loads / stores / stack variables.
        //  Make a new virtual variable.
        //  if operand is Virtual or Register, a stack slot should already be allocated for it by now, find it.
        //  Insert load(new variable, stack slot) before instr.
        //  Change the instr to make it use the new variable.
        for operand in instr.operands_mut() {
            if let Some(operand_variable) = operand.to_variable() {
                if let Some(operand_non_stack) = operand_variable.to_non_stack() {
                    let stack_slot = ph
                        .data
                        .get_stack_slot_for(operand_non_stack)
                        .expect("stack slot should already be allocated");
                    let new_variable = Variable::Virtual(ph.data.acquire_next_virtual_id());
                    ph.insert_before(i, Instruction::load(new_variable, stack_slot));
                    *operand = new_variable.into();
                }
            }
        }
    });
}
