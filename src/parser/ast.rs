#[derive(Debug, Default)]
pub struct Module<'a> {
    pub procedures: Vec<Procedure<'a>>,
}

#[derive(Debug, Default)]
pub struct Procedure<'a> {
    pub name: &'a str,
    pub parameters: Vec<Parameter<'a>>,
    pub return_typ: Option<&'a str>,
    pub basic_blocks: Vec<BasicBlock<'a>>,
}

#[derive(Debug, Default)]
pub struct Parameter<'a> {
    pub name: &'a str,
    pub typ: &'a str,
}

#[derive(Debug, Default)]
pub struct BasicBlock<'a> {
    pub name: &'a str,
    pub parameters: Vec<Parameter<'a>>,
    pub instructions: Vec<Instruction<'a>>,
}

#[derive(Debug, Default)]
pub struct Instruction<'a> {
    pub opcode: &'a str,
    pub operands: Vec<&'a str>,
    pub destination: Option<&'a str>,
    pub target_block: Option<&'a str>,
    pub condition: Option<&'a str>,
}
