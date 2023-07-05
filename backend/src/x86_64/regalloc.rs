use std::collections::HashMap;

use crate::{
    instruction::Operand,
    module::{Procedure, Variable},
    regalloc::InstructionConstraint,
};

use super::Isa;

pub fn allocate_registers(proc: &mut Procedure) {
    let mut available_registers = Isa::register_ids().to_vec();
    available_registers.reverse();

    let mut virtual_to_register: HashMap<Variable, Variable> = HashMap::new();

    for block in proc.basic_blocks.iter_mut() {
        for instr in &mut block.instructions {
            let constraint = Isa::instruction_constraint(instr.opcode);
            let first_operand_is_also_dst = matches!(
                constraint,
                Some(InstructionConstraint::FirstOperandIsAlsoDestination)
            );

            for (i, operand) in instr.src.iter_mut().enumerate() {
                let Some(operand_var) = operand.to_variable() else { continue };

                if !operand_var.is_virtual() {
                    continue;
                }

                let register = *virtual_to_register.get(&operand_var).unwrap();
                *operand = Operand::Var(register);

                if i == 0 && first_operand_is_also_dst {
                    let dst_var = instr.dst.expect("constraint implies instr has a dst");
                    virtual_to_register.insert(dst_var, register);
                    instr.dst = Some(register);
                }
            }

            if !first_operand_is_also_dst {
                if let Some(dst) = &mut instr.dst {
                    if dst.is_virtual() {
                        let register_id = available_registers.pop().unwrap();
                        let register = Variable::Register(register_id);
                        virtual_to_register.insert(*dst, register);
                        *dst = register;
                    }
                }
            }

            if let Some(cond) = &mut instr.cond {
                for operand in cond.operands_mut() {
                    let Some(operand_var) = operand.to_variable() else { continue };

                    if !operand_var.is_virtual() {
                        continue;
                    }

                    let register = *virtual_to_register.get(&operand_var).unwrap();
                    *operand = Operand::Var(register);
                }
            }
        }
    }
}
