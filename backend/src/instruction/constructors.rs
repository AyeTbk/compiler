use crate::procedure::{StackId, Variable};

use super::{Instruction, Opcode, Operand, SourceOperands};

impl Instruction {
    pub fn invalid() -> Instruction {
        Self {
            opcode: Opcode::InvalidInstruction,
            src: SourceOperands::none(),
            dst: None,
            target: None,
            cond: None,
        }
    }

    pub fn store(dest: StackId, src: impl Into<Operand>) -> Instruction {
        Self {
            opcode: Opcode::Store,
            src: SourceOperands::from_iter([src.into()]),
            dst: Some(Variable::Stack(dest)),
            target: None,
            cond: None,
        }
    }

    pub fn load(dest: Variable, src: StackId) -> Instruction {
        Self {
            opcode: Opcode::Load,
            src: SourceOperands::from_iter([Operand::Var(Variable::Stack(src))]),
            dst: Some(dest),
            target: None,
            cond: None,
        }
    }

    pub fn mov(dest: Variable, src: impl Into<Operand>) -> Instruction {
        Self {
            opcode: Opcode::Move,
            src: SourceOperands::from_iter([src.into()]),
            dst: Some(dest),
            target: None,
            cond: None,
        }
    }
}
