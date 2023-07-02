use crate::module::{StackSlotId, Variable};

use super::{Instruction, Opcode, Operand, SourceOperands};

impl Instruction {
    pub fn store(dest: StackSlotId, src: Variable) -> Instruction {
        Self {
            opcode: Opcode::Store,
            src: SourceOperands::from_iter([Operand::Var(src)]),
            dst: Some(Variable::Stack(dest)),
            target_block: None,
            cond: None,
        }
    }

    pub fn load(dest: Variable, src: StackSlotId) -> Instruction {
        Self {
            opcode: Opcode::Load,
            src: SourceOperands::from_iter([Operand::Var(Variable::Stack(src))]),
            dst: Some(dest),
            target_block: None,
            cond: None,
        }
    }
}
