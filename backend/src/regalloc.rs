use crate::{
    instruction::{Instruction, Opcode},
    module::Procedure,
    utils::Peephole,
};

pub enum InstructionConstraint {
    FirstOperandIsDestination,
}

/// TODO write up of what it is doing conceptually, so I can know what it's trying to do
/// and why it's supposed to work...
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
        // If dest is some and not stack:
        //  allocate a stack slot for it
        //  insert store(stack slot, dest) after instruction.
        if let Some(dest) = instr.dst {
            if let Some(dest_non_stack) = dest.to_non_stack() {
                let stack_slot = ph.data.allocate_stack_slot_for(dest_non_stack);
                ph.insert_before(0, Instruction::store(stack_slot, dest));
            }
        }

        // For every operand:
        //  dont do anything on loads / stores / stack variables.
        //  if operand is Virtual or Register, a stack slot should already be allocated for it by now, find it.
        //  insert load(original operand, stack slot) before instr.
        for operand in instr.operands() {
            match instr.opcode {
                Opcode::Load | Opcode::Store => (),
                _ => continue,
            }

            if let Some(operand_variable) = operand.to_variable() {
                if let Some(operand_non_stack) = operand_variable.to_non_stack() {
                    let stack_slot = ph
                        .data
                        .get_stack_slot_for(operand_non_stack)
                        .expect("stack slot should already be allocated");
                    ph.insert_before(i, Instruction::load(operand_variable, stack_slot));
                }
            }
        }
    });
}
