use std::collections::HashMap;

use crate::{
    callconv::{CallingConvention, CallingConventionId},
    instruction::Opcode,
    procedure::RegisterId,
    regalloc::InstructionConstraint,
};

pub struct Isa {
    pub register_ids: Vec<RegisterId>,
    pub instr_constraints: HashMap<Opcode, InstructionConstraint>,
    pub callconvs: HashMap<CallingConventionId, CallingConvention>,
}

impl Isa {
    pub fn register_ids(&self) -> &[RegisterId] {
        &self.register_ids
    }

    pub fn instruction_constraint(&self, opcode: Opcode) -> Option<InstructionConstraint> {
        self.instr_constraints.get(&opcode).copied()
    }

    pub fn calling_convention(
        &self,
        callconv_id: CallingConventionId,
    ) -> Option<&CallingConvention> {
        self.callconvs.get(&callconv_id)
    }
}
