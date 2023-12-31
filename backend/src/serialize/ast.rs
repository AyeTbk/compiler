#[derive(Debug, Default)]
pub struct Module<'a> {
    pub data: Vec<Data<'a>>,
    pub external_procedures: Vec<ProcedureSignature<'a>>,
    pub procedures: Vec<Procedure<'a>>,
}

#[derive(Debug)]
pub struct Data<'a> {
    pub name: Span<'a>,
    pub typ: Span<'a>,
    pub value: Span<'a>,
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
    pub return_type: Option<Span<'a>>,
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
    pub destination: Option<Destination<'a>>,
    pub target: Option<Target<'a>>,
    pub condition: Option<Condition<'a>>,
}

#[derive(Debug)]
pub struct Destination<'a> {
    pub name: Span<'a>,
    pub typ: Option<Span<'a>>,
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
