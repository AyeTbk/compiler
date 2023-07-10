use crate::procedure::Variable;

mod constructors;
mod opcode;
pub use opcode::Opcode;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub src: SourceOperands,
    pub dst: Option<Variable>, // TODO Consider supporting multiple returns
    pub target: Option<Target>,
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
    Imm(u64),
}

impl Operand {
    pub fn is_immediate(self) -> bool {
        match self {
            Self::Imm(_) => true,
            _ => false,
        }
    }

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
pub enum Target {
    Block(String),
    Procedure(String),
}

impl Target {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Block(s) | Self::Procedure(s) => s,
        }
    }

    pub fn as_block(&self) -> Option<&str> {
        match self {
            Self::Block(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_procedure(&self) -> Option<&str> {
        match self {
            Self::Procedure(s) => Some(s),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Condition {
    Equals(Operand, Operand),
    NotEquals(Operand, Operand),
}

impl Condition {
    pub fn operands(&self) -> [Operand; 2] {
        match self {
            Self::Equals(a, b) => [*a, *b],
            Self::NotEquals(a, b) => [*a, *b],
        }
    }

    pub fn operands_mut(&mut self) -> [&mut Operand; 2] {
        match self {
            Self::Equals(a, b) => [a, b],
            Self::NotEquals(a, b) => [a, b],
        }
    }
}