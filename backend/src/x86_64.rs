use crate::{instruction::Opcode, regalloc::InstructionConstraint};

#[repr(u32)]
pub enum Register {
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

pub struct Isa;

impl Isa {
    pub fn registers() -> &'static [u32] {
        use Register::*;
        &[
            Rax as u32, Rbx as u32, Rcx as u32, Rdx as u32, Rbp as u32, Rsp as u32, Rsi as u32,
            Rdi as u32, R8 as u32, R9 as u32, R10 as u32, R11 as u32, R12 as u32, R13 as u32,
            R14 as u32, R15 as u32,
        ]
    }

    pub fn instruction_constraint(opcode: Opcode) -> Option<InstructionConstraint> {
        match opcode {
            Opcode::Add | Opcode::Sub => Some(InstructionConstraint::FirstOperandIsDestination),
            _ => None,
        }
    }
}
