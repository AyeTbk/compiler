use crate::{callconv::CallingConvention, declarations::Declarations, isa::Isa};

pub struct Context {
    pub isa: Isa,
    pub declarations: Declarations,
}

impl Context {
    pub fn new(isa: Isa) -> Self {
        Self {
            isa,
            declarations: Default::default(),
        }
    }

    pub fn proc_callconv(&self, proc_name: &str) -> Option<&CallingConvention> {
        let proc = self.declarations.get_procedure(proc_name)?;
        self.isa.calling_convention(proc.calling_convention?)
    }
}
