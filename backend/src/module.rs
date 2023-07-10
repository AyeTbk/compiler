use crate::{declarations::Declarations, procedure::Procedure};

#[derive(Debug, Default)]
pub struct Module {
    pub declarations: Declarations,
    pub procedures: Vec<Procedure>,
}
