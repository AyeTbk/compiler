use std::collections::HashMap;

use crate::{
    callconv::CallingConventionId,
    interner::{Interner, Symbol},
    procedure::{ExternalProcedure, Procedure},
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

    pub fn declare_procedure(&mut self, procedure: &Procedure) {
        let name = self.symbols.get_or_intern(&procedure.signature.name);
        let parameters = procedure
            .blocks
            .entry_parameters()
            .map(|_param| ProcedureParameter { typ: () })
            .collect();
        let return_type = procedure
            .data
            .return_type
            .as_ref()
            .map(|_ret| ProcedureReturn { typ: () });

        let procdecl = ProcedureDeclaration {
            external: false,
            name,
            parameters,
            return_type,
            calling_convention: procedure.signature.calling_convention,
        };

        self.procedures.insert(name, procdecl);
    }

    pub fn declare_external_procedure(&mut self, procedure: &ExternalProcedure) {
        let name = self.symbols.get_or_intern(&procedure.name);
        let parameters = procedure
            .parameters
            .iter()
            .map(|_param| ProcedureParameter { typ: () })
            .collect();
        let return_type = procedure
            .return_type
            .as_ref()
            .map(|_ret| ProcedureReturn { typ: () });

        let procdecl = ProcedureDeclaration {
            external: false,
            name,
            parameters,
            return_type,
            calling_convention: Some(procedure.calling_convention),
        };

        self.procedures.insert(name, procdecl);
    }
}

#[derive(Debug)]
pub struct ProcedureDeclaration {
    pub external: bool,
    pub name: Symbol,
    pub parameters: Vec<ProcedureParameter>,
    pub return_type: Option<ProcedureReturn>,
    pub calling_convention: Option<CallingConventionId>,
}

#[derive(Debug)]
pub struct ProcedureParameter {
    pub typ: (),
}

#[derive(Debug)]
pub struct ProcedureReturn {
    pub typ: (),
}
