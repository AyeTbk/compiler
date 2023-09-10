use std::collections::HashMap;

use crate::{
    callconv::{CallingConvention, CallingConventionId},
    instruction::Opcode,
    isa::Isa,
    procedure::RegisterId,
    regalloc::InstructionConstraint,
};

pub mod assembly;
pub mod mir;
pub mod regalloc;

pub fn make_isa() -> Isa {
    Isa {
        register_ids: Register::ids().iter().copied().collect(),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Size {
    Byte = 1,
    Word = 2,
    Dword = 4,
    Qword = 8,
}

impl TryFrom<u64> for Size {
    type Error = ();
    fn try_from(value: u64) -> Result<Self, Self::Error> {
        let variant = match value {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => return Err(()),
        };
        Ok(variant)
    }
}

macro_rules! define_registers {
    ($($variant:ident : $qstr:expr, $dstr:expr, $wstr:expr, $bstr:expr);* $(;)?) => {
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

            pub const fn variant(id: RegisterId) -> Self {
                Self::variants()[id as usize]
            }

            pub const fn name(&self, size: Size) -> &'static str {
                match size {
                    Size::Byte => match self {
                        $(Self::$variant => $bstr),*
                    },
                    Size::Word => match self {
                        $(Self::$variant => $wstr),*
                    },
                    Size::Dword => match self {
                        $(Self::$variant => $dstr),*
                    },
                    Size::Qword => match self {
                        $(Self::$variant => $qstr),*
                    },
                }
            }

            pub fn from_str(s: &str) -> Option<Self> {
                $(
                    if s.eq_ignore_ascii_case(Self::$variant.name(Size::Qword)) ||
                        s.eq_ignore_ascii_case(Self::$variant.name(Size::Dword)) ||
                        s.eq_ignore_ascii_case(Self::$variant.name(Size::Word)) ||
                        s.eq_ignore_ascii_case(Self::$variant.name(Size::Byte))
                    {
                        return Some(Self::$variant);
                    }
                )*

                None
            }
        }
    };
}

define_registers! {
    Rax: "rax", "eax", "ax", "al";
    Rbx: "rbx", "ebx", "bx", "bl";
    Rcx: "rcx", "ecx", "cx", "cl";
    Rdx: "rdx", "edx", "dx", "dl";
    Rbp: "rbp", "ebp", "bp", "bpl";
    Rsp: "rsp", "esp", "sp", "spl";
    Rsi: "rsi", "esi", "si", "sil";
    Rdi: "rdi", "edi", "di", "dil";
    R8: "r8", "r8d", "r8w", "r8b";
    R9: "r9", "r9d", "r9w", "r9b";
    R10: "r10", "r10d", "r10w", "r10b";
    R11: "r11", "r11d", "r11w", "r11b";
    R12: "r12", "r12d", "r12w", "r12b";
    R13: "r13", "r13d", "r13w", "r13b";
    R14: "r14", "r14d", "r14w", "r14b";
    R15: "r15", "r15d", "r15w", "r15b";
    //
    Rip: "rip", "eip", "ip", "ip";
}
