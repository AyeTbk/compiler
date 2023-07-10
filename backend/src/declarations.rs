use std::collections::HashMap;

use crate::{
    calling_convention::CallingConvention,
    interner::{Interner, Symbol},
    procedure::Signature,
};

#[derive(Debug, Default)]
pub struct Declarations {
    pub symbols: Interner,
    pub procedures: HashMap<Symbol, ProcedureDeclaration>,
    pub variables: (),
    pub constants: (),
}

impl Declarations {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_procedure(&self, name: &str) -> Option<&ProcedureDeclaration> {
        let Some(symbol) = self.symbols.get(name) else { return None; };
        self.procedures.get(&symbol)
    }

    pub fn declare_procedure(&mut self, signature: &Signature) {
        let name = self.symbols.get_or_intern(&signature.name);
        let parameters = signature
            .parameters
            .iter()
            .map(|_param| ProcedureParameter { typ: () })
            .collect();
        let returns = signature
            .returns
            .iter()
            .map(|_ret| ProcedureReturn { typ: () })
            .collect();

        let procdecl = ProcedureDeclaration {
            external: false,
            name,
            parameters,
            returns,
            calling_convention: signature.calling_convention,
        };

        self.procedures.insert(name, procdecl);
    }
}

#[derive(Debug)]
pub struct ProcedureDeclaration {
    pub external: bool,
    pub name: Symbol,
    pub parameters: Vec<ProcedureParameter>,
    pub returns: Vec<ProcedureReturn>,
    pub calling_convention: Option<CallingConvention>,
}

#[derive(Debug)]
pub struct ProcedureParameter {
    pub typ: (),
}

#[derive(Debug)]
pub struct ProcedureReturn {
    pub typ: (),
}
