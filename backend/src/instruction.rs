use crate::module::Variable;

mod constructors;
mod opcode;
pub use opcode::Opcode;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub src: SourceOperands,
    pub dst: Option<Variable>,
    pub target_block: Option<String>,
    pub cond: Option<Condition>,
}

impl Instruction {
    pub fn operands(&self) -> impl Iterator<Item = &Operand> {
        self.src.operands.iter()
    }

    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut Operand> {
        self.src.operands.iter_mut()
    }
}

#[derive(Debug)]
pub struct SourceOperands {
    pub operands: Vec<Operand>,
}

impl SourceOperands {
    pub fn none() -> Self {
        Self {
            operands: Vec::new(),
        }
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Operand> {
        self.operands.iter_mut()
    }
}

impl FromIterator<Operand> for SourceOperands {
    fn from_iter<T: IntoIterator<Item = Operand>>(iter: T) -> Self {
        Self {
            operands: iter.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Var(Variable),
    Imm(u32),
}

impl Operand {
    pub fn to_variable(self) -> Option<Variable> {
        match self {
            Self::Var(var) => Some(var),
            _ => None,
        }
    }
}

impl From<Variable> for Operand {
    fn from(value: Variable) -> Self {
        Self::Var(value)
    }
}

#[derive(Debug)]
pub enum Condition {
    Equals(Operand, Operand),
    NotEquals(Operand, Operand),
}

impl Condition {
    pub fn operands_mut(&mut self) -> impl Iterator<Item = &mut Operand> {
        match self {
            Self::Equals(a, b) => [a, b].into_iter(),
            Self::NotEquals(a, b) => [a, b].into_iter(),
        }
    }
}
