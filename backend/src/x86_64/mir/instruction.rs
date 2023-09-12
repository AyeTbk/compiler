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
    pub fn size(&self) -> Size {
        match self {
            Self::Register(reg) => reg.size,
            Self::RegisterRegister(reg1, _) => reg1.size,
            Self::RegisterMemory(reg, _) => reg.size,
            Self::RegisterImmediate(reg, _) => reg.size,
            Self::Memory(mem) => mem.size,
            Self::MemoryRegister(mem, _) => mem.size,
            Self::MemoryImmediate(mem, _) => mem.size,
            Self::Immediate(imm) => imm.size,
            _ => Size::Word,
        }
    }

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

    pub fn set_sizes_two(&mut self, size1: Size, size2: Size) {
        match self {
            Self::RegisterRegister(reg1, reg2) => {
                reg1.size = size1;
                reg2.size = size2
            }
            Self::RegisterMemory(reg, mem) => {
                reg.size = size1;
                mem.size = size2;
            }
            Self::RegisterImmediate(reg, imm) => {
                reg.size = size1;
                imm.size = size2;
            }
            Self::MemoryRegister(mem, reg) => {
                mem.size = size1;
                reg.size = size2;
            }
            Self::MemoryImmediate(mem, imm) => {
                mem.size = size1;
                imm.size = size2;
            }
            _ => panic!("cannot set size_two"),
        }
    }

    pub fn are_same_two(&self) -> bool {
        match self {
            Self::RegisterRegister(reg1, reg2) => reg1.register == reg2.register,
            _ => false,
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

impl Condition {
    pub fn operands(&self) -> Operands {
        match self {
            Self::Equals(op) | Self::NotEquals(op) => op.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Register(SizedRegister),
    Memory(SizedMemory),
    Immediate(Immediate),
}
