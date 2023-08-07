#[derive(Debug)]
pub struct Data {
    pub value: Value,
}

impl Data {
    pub fn new(value: Value) -> Self {
        Data { value }
    }
}

#[derive(Debug)]
pub enum Value {
    U64(u64),
    Bytes(Vec<u8>),
}
