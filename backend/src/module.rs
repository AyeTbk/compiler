use crate::instruction::Instruction;

pub type VirtualId = u32;
pub type RegisterId = u32;
pub type StackSlotId = u32;

#[derive(Debug, Default)]
pub struct Module {
    pub procedures: Vec<Procedure>,
}

#[derive(Debug)]
pub struct Procedure {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub returns: Vec<Parameter>,
    pub blocks: Blocks,
    pub data: ProcedureData,
    pub calling_convention: Option<CallingConvention>,
}

#[derive(Debug, Default)]
pub struct ProcedureData {
    pub stack_slots: Vec<StackSlot>,
    pub highest_virtual_id: VirtualId,
}

impl ProcedureData {
    pub fn allocate_local_stack_slot_for(&mut self, variable: VariableNonStack) -> StackSlotId {
        let id = self.stack_slots.len();
        let stack_slot = StackSlot {
            typ: Typ,
            kind: StackSlotKind::Local {
                allocated_for: variable,
            },
        };
        self.stack_slots.push(stack_slot);
        id.try_into().unwrap()
    }

    pub fn allocate_caller_stack_slot(&mut self) -> StackSlotId {
        let id = self.stack_slots.len();
        let stack_slot = StackSlot {
            typ: Typ,
            kind: StackSlotKind::Caller,
        };
        self.stack_slots.push(stack_slot);
        id.try_into().unwrap()
    }

    pub fn get_stack_slot_for(&self, variable: VariableNonStack) -> Option<StackSlotId> {
        self.stack_slots
            .iter()
            .enumerate()
            .find(|(_, ss)| match ss.kind {
                StackSlotKind::Local { allocated_for } => allocated_for == variable,
                StackSlotKind::Caller => false,
            })
            .map(|(i, _)| i as StackSlotId)
    }

    pub fn combined_local_stack_slots_size(&self) -> u64 {
        (self.local_stack_slots().count() * std::mem::size_of::<u64>()) as u64
    }

    pub fn stack_slot_memory_offset(&self, stack_slot: StackSlotId) -> u64 {
        self.local_stack_slots()
            .enumerate()
            .take_while(|(i, _)| *i as u32 != stack_slot)
            .map(|_| std::mem::size_of::<u64>() as u64)
            .sum()
    }

    pub fn acquire_new_virtual_variable(&mut self) -> Variable {
        Variable::Virtual(self.acquire_next_virtual_id())
    }

    fn acquire_next_virtual_id(&mut self) -> VirtualId {
        self.highest_virtual_id = self.highest_virtual_id.checked_add(1).unwrap();
        self.highest_virtual_id
    }

    fn local_stack_slots(&self) -> impl Iterator<Item = &StackSlot> {
        self.stack_slots
            .iter()
            .filter(|ss| matches!(ss.kind, StackSlotKind::Local { .. }))
    }
}

#[derive(Debug)]
pub struct StackSlot {
    pub typ: Typ,
    pub kind: StackSlotKind,
}

#[derive(Debug)]
pub enum StackSlotKind {
    Local { allocated_for: VariableNonStack },
    Caller,
}

#[derive(Debug)]
pub struct Blocks {
    pub entry: Block,
    pub others: Vec<Block>,
}

impl Blocks {
    pub fn iter(&self) -> impl Iterator<Item = &Block> {
        Some(&self.entry).into_iter().chain(self.others.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Block> {
        Some(&mut self.entry)
            .into_iter()
            .chain(self.others.iter_mut())
    }
}

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub variable: Variable,
    pub typ: Typ,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variable {
    Virtual(VirtualId),
    Register(RegisterId),
    Stack(StackSlotId),
}

impl Variable {
    pub fn is_virtual(&self) -> bool {
        match self {
            Variable::Virtual(_) => true,
            _ => false,
        }
    }

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallingConvention {
    AllStack,
    SysV,
}
