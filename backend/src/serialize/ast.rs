#[derive(Debug, Default)]
pub struct Module<'a> {
    pub external_procedures: Vec<ProcedureSignature<'a>>,
    pub procedures: Vec<Procedure<'a>>,
}

#[derive(Debug)]
pub struct Procedure<'a> {
    pub signature: ProcedureSignature<'a>,
    pub blocks: Vec<Block<'a>>,
}

#[derive(Debug)]
pub struct ProcedureSignature<'a> {
    pub name: Span<'a>,
    pub parameters: Vec<Parameter<'a>>,
    pub returns: Vec<Span<'a>>,
}

#[derive(Debug)]
pub struct Parameter<'a> {
    pub name: Option<Span<'a>>,
    pub typ: Span<'a>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub name: Span<'a>,
    pub parameters: Vec<Parameter<'a>>,
    pub instructions: Vec<Instruction<'a>>,
}

#[derive(Debug)]
pub struct Instruction<'a> {
    pub opcode: Span<'a>,
    pub operands: Vec<Span<'a>>,
    pub destination: Option<Span<'a>>,
    pub target: Option<Target<'a>>,
    pub condition: Option<Condition<'a>>,
}

#[derive(Debug)]
pub struct Target<'a> {
    pub sigil: Span<'a>,
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
