use crate::instruction::Instruction;

pub type VirtualId = u32;
pub type RegisterId = u32;
pub type StackSlotId = u32;
pub type ImmediateId = u32;

#[derive(Debug, Default)]
pub struct Module {
    pub procedures: Vec<Procedure>,
}

#[derive(Debug)]
pub struct Procedure {
    pub name: String,
    pub return_typ: Typ,
    pub basic_blocks: BasicBlocks,
    pub data: ProcedureData,
}

#[derive(Debug)]
pub struct BasicBlocks {
    pub entry: BasicBlock,
    pub others: Vec<BasicBlock>,
}

impl BasicBlocks {
    pub fn iter(&mut self) -> impl Iterator<Item = &BasicBlock> {
        Some(&self.entry).into_iter().chain(self.others.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut BasicBlock> {
        Some(&mut self.entry)
            .into_iter()
            .chain(self.others.iter_mut())
    }
}

#[derive(Debug, Default)]
pub struct ProcedureData {
    pub stack_slots: Vec<StackSlot>,
}

impl ProcedureData {
    pub fn allocate_stack_slot_for(&mut self, variable: VariableNonStack) -> StackSlotId {
        let id = self.stack_slots.len();
        let stack_slot = StackSlot {
            typ: Typ,
            allocated_for: variable,
        };
        self.stack_slots.push(stack_slot);
        id.try_into().unwrap()
    }

    pub fn get_stack_slot_for(&self, variable: VariableNonStack) -> Option<StackSlotId> {
        self.stack_slots
            .iter()
            .enumerate()
            .find(|(_, ss)| ss.allocated_for == variable)
            .map(|(i, _)| i as StackSlotId)
    }
}

#[derive(Debug)]
pub struct StackSlot {
    pub typ: Typ,
    pub allocated_for: VariableNonStack,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Parameter {
    pub variable: Variable,
    pub typ: Typ,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variable {
    Virtual(VirtualId),
    Register(RegisterId),
    Stack(StackSlotId),
}

impl Variable {
    pub fn as_stack(&self) -> Option<StackSlotId> {
        match self {
            Variable::Stack(id) => Some(*id),
            _ => None,
        }
    }

    pub fn to_non_stack(self) -> Option<VariableNonStack> {
        match self {
            Self::Register(id) => Some(VariableNonStack::Register(id)),
            Self::Virtual(id) => Some(VariableNonStack::Virtual(id)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableNonStack {
    Virtual(VirtualId),
    Register(RegisterId),
}

impl VariableNonStack {
    pub fn to_variable(&self) -> Variable {
        match *self {
            Self::Virtual(id) => Variable::Virtual(id),
            Self::Register(id) => Variable::Register(id),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Typ;
