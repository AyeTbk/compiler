use crate::r#type::{IntegerType, Type};

#[derive(Debug)]
pub struct Data {
    pub value: Value,
}

impl Data {
    pub fn new(value: Value) -> Self {
        Data { value }
    }

    pub fn typ(&self) -> Type {
        match &self.value {
            Value::U64(_) => Type::Integer(IntegerType::U64),
            Value::Bytes(_) => Type::Bytes,
        }
    }
}

#[derive(Debug)]
pub enum Value {
    U64(u64),
    Bytes(Vec<u8>),
}
