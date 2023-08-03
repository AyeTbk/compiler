use crate::procedure::{ExternalProcedure, Procedure};

#[derive(Debug, Default)]
pub struct Module {
    pub external_procedures: Vec<ExternalProcedure>,
    pub data: Vec<Data>,
    pub procedures: Vec<Procedure>,
}

#[derive(Debug)]
pub struct Data {
    pub value: u64,
}

impl Data {
    pub fn new(value: u64) -> Self {
        Data { value }
    }
}
