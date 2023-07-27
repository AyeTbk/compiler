use crate::procedure::Procedure;

#[derive(Debug, Default)]
pub struct Module {
    pub procedures: Vec<Procedure>,
}
