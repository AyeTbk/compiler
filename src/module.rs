use crate::instruction::Instruction;

#[derive(Debug, Default)]
pub struct Module {
    pub procedures: Vec<Procedure>,
}

#[derive(Debug, Default)]
pub struct Procedure {
    pub return_typ: Typ,
    pub entry_block: BasicBlock,
    pub other_blocks: Vec<BasicBlock>,
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Default)]
pub struct Parameter {
    pub register: u32,
    pub typ: Typ,
}

#[derive(Debug, Default)]
pub struct Typ;
