use crate::procedure::{ExternalProcedure, Procedure};

#[derive(Debug, Default)]
pub struct Module {
    pub external_procedures: Vec<ExternalProcedure>,
    pub procedures: Vec<Procedure>,
}
