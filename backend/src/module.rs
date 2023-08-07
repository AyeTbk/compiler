use crate::{
    data::Data,
    procedure::{ExternalProcedure, Procedure},
};

#[derive(Debug, Default)]
pub struct Module {
    pub external_procedures: Vec<ExternalProcedure>,
    pub data: Vec<Data>,
    pub procedures: Vec<Procedure>,
}
