use crate::{instruction::Opcode, module::RegisterId, regalloc::InstructionConstraint};
use macros::stringify_lowercase;

pub mod assembly;
pub mod regalloc;

pub struct Isa;

impl Isa {
    pub fn register_ids() -> &'static [u32] {
        use Register::*;
        &[
            Rax as u32, Rbx as u32, Rcx as u32, Rdx as u32, Rbp as u32, Rsp as u32, Rsi as u32,
            Rdi as u32, R8 as u32, R9 as u32, R10 as u32, R11 as u32, R12 as u32, R13 as u32,
            R14 as u32, R15 as u32,
        ]
    }

    pub fn register_name(register_id: RegisterId) -> &'static str {
        Register::variants()[register_id as usize].as_str()
    }

    pub fn instruction_constraint(opcode: Opcode) -> Option<InstructionConstraint> {
        match opcode {
            Opcode::Add | Opcode::Sub => Some(InstructionConstraint::FirstOperandIsAlsoDestination),
            _ => None,
        }
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
