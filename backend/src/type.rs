use crate::context::Context;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer(IntegerType),
    Bytes,
    Address,
}

impl Type {
    pub fn from_str(s: &str) -> Option<Self> {
        match IntegerType::from_str(s) {
            Some(int) => Some(Self::Integer(int)),
            None => match s {
                "bytes" => Some(Self::Bytes),
                "addr" => Some(Self::Address),
                _ => None,
            },
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Integer(int) => int.as_str(),
            Self::Bytes => "bytes",
            Self::Address => "addr",
        }
    }

    pub fn sizeof(&self, _context: &Context) -> u64 {
        match self {
            Self::Integer(int) => int.sizeof(),
            Self::Address => 8, // TODO get it from the context instead of assuming 64bit address space
            Self::Bytes => 8,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerType {
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
}

impl IntegerType {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "u64" => Some(IntegerType::U64),
            "i64" => Some(IntegerType::I64),
            "u32" => Some(IntegerType::U32),
            "i32" => Some(IntegerType::I32),
            "u16" => Some(IntegerType::U16),
            "i16" => Some(IntegerType::I16),
            "u8" => Some(IntegerType::U8),
            "i8" => Some(IntegerType::I8),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::U64 => "u64",
            Self::I64 => "i64",
            Self::U32 => "u32",
            Self::I32 => "i32",
            Self::U16 => "u16",
            Self::I16 => "i16",
            Self::U8 => "u8",
            Self::I8 => "i8",
        }
    }

    pub fn sizeof(&self) -> u64 {
        match self {
            Self::U64 => 8,
            Self::I64 => 8,
            Self::U32 => 4,
            Self::I32 => 4,
            Self::U16 => 2,
            Self::I16 => 2,
            Self::U8 => 1,
            Self::I8 => 1,
        }
    }
}
