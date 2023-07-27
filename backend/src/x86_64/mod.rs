use std::collections::HashMap;

use crate::{
    callconv::{CallingConvention, CallingConventionId},
    instruction::Opcode,
    isa::Isa,
    procedure::RegisterId,
    regalloc::InstructionConstraint,
};
use macros::stringify_lowercase;

pub mod assembly;
pub mod regalloc;

pub fn make_isa() -> Isa {
    Isa {
        register_ids: Register::ids().iter().copied().collect(),
        register_names: Register::variants().iter().map(|r| r.as_str()).collect(),
        instr_constraints: {
            let mut cnst = HashMap::new();
            use InstructionConstraint::*;
            cnst.insert(Opcode::Add, FirstOperandIsAlsoDestination);
            cnst.insert(Opcode::Sub, FirstOperandIsAlsoDestination);
            cnst
        },
        callconvs: {
            let mut cllcnv = HashMap::new();
            use Register::*;
            cllcnv.insert(
                CallingConventionId::SysV,
                CallingConvention {
                    id: CallingConventionId::SysV,
                    integer_parameter_registers: [Rdi, Rsi, Rdx, Rcx, R8, R9]
                        .into_iter()
                        .map(|r| r as RegisterId)
                        .collect(),
                    integer_return_register: Rax as RegisterId,
                    preserved_registers: [Rbx, Rbp, Rsp, R12, R13, R14, R15]
                        .into_iter()
                        .map(|r| r as RegisterId)
                        .collect(),
                    scratch_registers: [Rax, Rcx, Rdx, Rdi, Rsi, R8, R9, R10, R11]
                        .into_iter()
                        .map(|r| r as RegisterId)
                        .collect(),
                },
            );
            cllcnv
        },
    }
}

macro_rules! define_registers {
    ($($variant:ident),* $(,)?) => {
        #[repr(u32)]
        #[derive(Debug, Clone, Copy)]
        pub enum Register {
            $($variant),*
        }

        impl Register {
            pub const fn variants() -> &'static [Self] {
                &[
                    $(Self::$variant),*
                ]
            }

            pub const fn ids() -> &'static [RegisterId] {
                &[
                    $(Self::$variant as RegisterId),*
                ]
            }

            pub const fn as_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => stringify_lowercase!($variant)),*
                }
            }

            pub fn from_str(s: &str) -> Option<Self> {
                $(
                    if s.eq_ignore_ascii_case(Self::$variant.as_str()) {
                        return Some(Self::$variant);
                    }
                )*

                None
            }
        }
    };
}

define_registers! {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rbp,
    Rsp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}
