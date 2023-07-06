use crate::module::{StackSlotId, Variable};

use super::{Instruction, Opcode, Operand, SourceOperands};

impl Instruction {
    pub fn invalid() -> Instruction {
        Self {
            opcode: Opcode::InvalidInstruction,
            src: SourceOperands::none(),
            dst: None,
            target_block: None,
            cond: None,
        }
    }

    pub fn store(dest: StackSlotId, src: impl Into<Operand>) -> Instruction {
        Self {
            opcode: Opcode::Store,
            src: SourceOperands::from_iter([src.into()]),
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
