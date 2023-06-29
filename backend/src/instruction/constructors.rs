use crate::module::{StackSlotId, Variable};

use super::{Instruction, Opcode, SourceOperand, SourceOperands};

impl Instruction {
    pub fn store(dest: StackSlotId, src: Variable) -> Instruction {
        Self {
            opcode: Opcode::Store,
            src: SourceOperands::from_iter([SourceOperand::Var(src)]),
            dst: Some(Variable::Stack(dest)),
        }
    }

    pub fn load(dest: Variable, src: StackSlotId) -> Instruction {
        Self {
            opcode: Opcode::Load,
            src: SourceOperands::from_iter([SourceOperand::Var(Variable::Stack(src))]),
            dst: Some(dest),
        }
    }
}
