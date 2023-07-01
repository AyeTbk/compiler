use crate::module::Variable;

mod constructors;
mod opcode;
pub use opcode::Opcode;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub src: SourceOperands,
    pub dst: Option<Variable>,
    pub target_block: Option<TargetBlock>,
    pub cond: Option<Condition>,
}

impl Instruction {
    pub fn operands(&self) -> impl Iterator<Item = &SourceOperand> {
        self.src.operands.iter()
    }
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

#[derive(Debug)]
pub struct TargetBlock {
    pub name: String,
    pub arguments: Vec<SourceOperand>,
}

#[derive(Debug)]
pub enum Condition {
    Equals(SourceOperand, SourceOperand),
    NotEquals(SourceOperand, SourceOperand),
}
