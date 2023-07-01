#[derive(Debug, Default)]
pub struct Module<'a> {
    pub procedures: Vec<Procedure<'a>>,
}

#[derive(Debug)]
pub struct Procedure<'a> {
    pub name: Span<'a>,
    pub parameters: Vec<Parameter<'a>>,
    pub return_typ: Option<Span<'a>>,
    pub basic_blocks: Vec<BasicBlock<'a>>,
}

#[derive(Debug)]
pub struct Parameter<'a> {
    pub name: Span<'a>,
    pub typ: Span<'a>,
}

#[derive(Debug)]
pub struct BasicBlock<'a> {
    pub name: Span<'a>,
    pub parameters: Vec<Parameter<'a>>,
    pub instructions: Vec<Instruction<'a>>,
}

#[derive(Debug)]
pub struct Instruction<'a> {
    pub opcode: Span<'a>,
    pub operands: Vec<Span<'a>>,
    pub destination: Option<Span<'a>>,
    pub target_block: Option<TargetBlock<'a>>,
    pub condition: Option<Condition<'a>>,
}

#[derive(Debug)]
pub struct TargetBlock<'a> {
    pub name: Span<'a>,
    pub arguments: Vec<Span<'a>>,
}

#[derive(Debug)]
pub enum Condition<'a> {
    Equals(Span<'a>, Span<'a>),
    NotEquals(Span<'a>, Span<'a>),
}

#[derive(Debug)]
pub struct Span<'a> {
    pub text: &'a str,
    pub from: usize,
    pub to: usize,
}
