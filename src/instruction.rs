use crate::module::Variable;

pub mod constructors;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub src: SourceOperands,
    pub dst: Option<Variable>,
}

impl Instruction {
    pub fn operands(&self) -> impl Iterator<Item = &SourceOperand> {
        self.src.operands.iter()
    }
}

#[derive(Debug)]
pub enum Opcode {
    Store,
    Load,
    Jump,
    Call,
    Add,
    Sub,
}

#[derive(Debug)]
pub struct SourceOperands {
    pub operands: Vec<SourceOperand>,
}

impl FromIterator<SourceOperand> for SourceOperands {
    fn from_iter<T: IntoIterator<Item = SourceOperand>>(iter: T) -> Self {
        Self {
            operands: iter.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SourceOperand {
    Var(Variable),
    Imm(u32),
}

impl SourceOperand {
    pub fn to_variable(self) -> Option<Variable> {
        match self {
            Self::Var(var) => Some(var),
            _ => None,
        }
    }
}
