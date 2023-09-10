use crate::{
    procedure::DataId,
    x86_64::{Register, Size},
};

use super::BlockId;

#[derive(Debug, Clone)]
pub enum Operands {
    ZeroOperand,
    Register(SizedRegister),
    RegisterRegister(SizedRegister, SizedRegister),
    RegisterMemory(SizedRegister, SizedMemory),
    RegisterImmediate(SizedRegister, Immediate),
    Memory(SizedMemory),
    MemoryRegister(SizedMemory, SizedRegister),
    MemoryImmediate(SizedMemory, Immediate),
    Immediate(Immediate),
    JumpTarget(JumpTarget),
}

impl Operands {
    pub fn size_one(&self) -> Option<Size> {
        match self {
            Self::Register(reg) => Some(reg.size),
            Self::Memory(mem) => Some(mem.size),
            Self::Immediate(imm) => Some(imm.size),
            _ => None,
        }
    }

    pub fn sizes_two(&self) -> Option<(Size, Size)> {
        match self {
            Self::RegisterRegister(reg1, reg2) => Some((reg1.size, reg2.size)),
            Self::RegisterMemory(reg, mem) => Some((reg.size, mem.size)),
            Self::RegisterImmediate(reg, imm) => Some((reg.size, imm.size)),
            Self::MemoryRegister(mem, reg) => Some((mem.size, reg.size)),
            Self::MemoryImmediate(mem, imm) => Some((mem.size, imm.size)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SizedRegister {
    pub size: Size,
    pub register: Register,
}

#[derive(Debug, Clone, Copy)]
pub struct SizedMemory {
    pub size: Size,
    pub memory: Memory,
}

#[derive(Debug, Clone, Copy)]
pub enum Memory {
    RegisterRelative { reg: Register, offset: Offset },
    DataRegisterRelative { reg: Register, data: DataId },
}

#[derive(Debug, Clone, Copy)]
pub struct Immediate {
    pub size: Size,
    pub bits: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum Offset {
    I8(i8),
    I32(i32),
}

impl Offset {
    pub fn as_i32(&self) -> i32 {
        match self {
            Offset::I8(n) => *n as i32,
            Offset::I32(n) => *n as i32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JumpTarget {
    BlockRelative(BlockId),
}

#[derive(Debug, Clone)]
pub enum Condition {
    Equals(Operands),
    NotEquals(Operands),
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Register(SizedRegister),
    Memory(SizedMemory),
    Immediate(Immediate),
}
