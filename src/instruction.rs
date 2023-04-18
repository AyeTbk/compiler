#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub src: SourceOperands,
    pub dst: DestinationOperand,
}

#[derive(Debug)]
pub enum Opcode {
    Add,
    Sub,
    Jump,
}

#[derive(Debug)]
pub struct SourceOperands {
    pub operands: Vec<SourceOperand>,
}

#[derive(Debug)]
pub enum SourceOperand {
    None,
    Reg(u32),
    Imm(u32),
}

#[derive(Debug)]
pub enum DestinationOperand {
    None,
    Reg(u32),
}
