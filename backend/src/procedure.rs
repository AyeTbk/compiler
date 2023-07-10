use crate::{calling_convention::CallingConvention, instruction::Instruction};

pub type VirtualId = u32;
pub type RegisterId = u32;
pub type StackId = u32;

#[derive(Debug, Clone)]
pub struct Signature {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub returns: Vec<Parameter>,
    pub calling_convention: Option<CallingConvention>,
}

#[derive(Debug)]
pub struct Procedure {
    pub signature: Signature,
    pub data: ProcedureData,
    pub blocks: Blocks,
}

#[derive(Debug, Default)]
pub struct ProcedureData {
    pub stack_data: StackData,
    pub highest_virtual_id: VirtualId,
}

impl ProcedureData {
    pub fn acquire_new_virtual_variable(&mut self) -> Variable {
        Variable::Virtual(self.acquire_next_virtual_id())
    }

    fn acquire_next_virtual_id(&mut self) -> VirtualId {
        self.highest_virtual_id = self.highest_virtual_id.checked_add(1).unwrap();
        self.highest_virtual_id
    }
}

#[derive(Debug, Default)]
pub struct StackData {
    pub vars: Vec<StackVar>,
    pub slots: Vec<StackSlot>,
    pub calls: Vec<StackCall>,
}

impl StackData {
    pub fn allocate_local_stack_slot(&mut self) -> StackId {
        let slot_id = self.slots.len();
        let stack_slot = StackSlot {
            typ: Typ,
            kind: StackSlotKind::Local,
        };
        self.slots.push(stack_slot);

        let id = self.vars.len();
        let var = StackVar::Slot(slot_id);
        self.vars.push(var);
        id.try_into().unwrap()
    }

    pub fn allocate_caller_stack_slot(&mut self) -> StackId {
        let slot_id = self.slots.len();
        let stack_slot = StackSlot {
            typ: Typ,
            kind: StackSlotKind::Caller,
        };
        self.slots.push(stack_slot);

        let id = self.vars.len();
        let var = StackVar::Slot(slot_id);
        self.vars.push(var);
        id.try_into().unwrap()
    }

    pub fn allocate_call_stack_var(&mut self, call_idx: u32, ordinal: u32) -> StackId {
        let id = self.vars.len();
        assert!((call_idx as usize) < self.calls.len());
        let var = StackVar::Call { call_idx, ordinal };
        self.vars.push(var);
        id.try_into().unwrap()
    }

    pub fn allocate_call(&mut self, size: u32) -> u32 {
        let id = self.calls.len();
        let stack_call = StackCall { size };
        self.calls.push(stack_call);
        id.try_into().unwrap()
    }

    pub fn total_local_stack_size(&self) -> u64 {
        let local_stack_slots_size =
            (self.local_stack_slots().count() * std::mem::size_of::<u64>()) as u64;
        let stack_call_size = self.calls.iter().map(|c| c.size).max().unwrap_or(0) as u64;
        local_stack_slots_size + stack_call_size
    }

    pub fn stack_var_memory_offset(&self, stack_id: StackId) -> i64 {
        // NOTE Currently works right-to-left
        // NOTE Assumes offsets for caller need to jump past a saved %rbp and the address pushed by the 'call', that is +16

        // Offset should be local to stack frame pointer.
        // Top_of_stack is (stack_frame_pointer - local_stack_size).
        // There are three cases:
        // 1. Call var
        // 2. Local slot var
        // 3. Caller slot var
        //
        // 1. Call var offsets:
        //   are negative,
        //   go up from top of stack,
        //   with higher cardinalities being closer to stack_frame_pointer.
        // 2. Local slot var offsets:
        //   are negative,
        //   go down from stack frame pointer,
        // 3. Caller slot var offsets:
        //   arg positive,
        //   go up from stack frame pointer

        todo!("rewrite this plz, so it works like the above comment says");

        // if self.is_local_stack_slot(stack_slot) {
        //     let mut offset = 0;
        //     for (i, _) in self.local_stack_slots().enumerate() {
        //         offset -= std::mem::size_of::<u64>() as i64;
        //         if i as u32 == stack_slot {
        //             break;
        //         }
        //     }
        //     offset
        // } else {
        //     let mut offset = 16;
        //     for (i, _) in self.caller_stack_slots().enumerate() {
        //         if i as u32 == stack_slot {
        //             break;
        //         }
        //         offset += std::mem::size_of::<u64>() as i64;
        //     }
        //     offset
        // }
    }

    pub fn is_local_stack_slot(&self, stack_slot: StackId) -> bool {
        self.local_stack_slots()
            .find(|(i, _)| *i as StackId == stack_slot)
            .is_some()
    }

    pub fn local_stack_slots(&self) -> impl Iterator<Item = (usize, &StackSlot)> {
        self.slots
            .iter()
            .enumerate()
            .filter(|(_, ss)| matches!(ss.kind, StackSlotKind::Local))
    }

    pub fn caller_stack_slots(&self) -> impl Iterator<Item = (usize, &StackSlot)> {
        self.slots
            .iter()
            .enumerate()
            .filter(|(_, ss)| matches!(ss.kind, StackSlotKind::Caller))
    }
}

#[derive(Debug)]
pub enum StackVar {
    Slot(usize),
    Call { call_idx: u32, ordinal: u32 },
}

#[derive(Debug)]
pub struct StackSlot {
    pub typ: Typ,
    pub kind: StackSlotKind,
}

#[derive(Debug)]
pub enum StackSlotKind {
    Local,
    Caller,
}

#[derive(Debug)]
pub struct StackCall {
    pub size: u32,
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
    Stack(StackId),
}

impl Variable {
    pub fn is_virtual(&self) -> bool {
        match self {
            Variable::Virtual(_) => true,
            _ => false,
        }
    }

    pub fn as_stack(&self) -> Option<StackId> {
        match self {
            Variable::Stack(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Typ;