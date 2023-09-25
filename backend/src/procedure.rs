use std::collections::{BTreeMap, HashMap};

use crate::{
    callconv::CallingConventionId, context::Context, instruction::Instruction, r#type::Type,
};

pub type VirtualId = u32;
pub type RegisterId = u32;
pub type StackId = u32;
pub type DataId = u32;

// TODO maybe put in procedure data?
#[derive(Debug, Clone)]
pub struct Signature {
    pub name: String,
    // pub parameters: Vec<Parameter>, // Find it in proc.blocks
    // pub returns: Vec<Type>,
    pub calling_convention: Option<CallingConventionId>,
}

#[derive(Debug)]
pub struct ExternalProcedure {
    pub name: String,
    pub parameters: Vec<Type>,
    pub return_type: Option<Type>,
    pub calling_convention: CallingConventionId,
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
    pub virtual_types: HashMap<VirtualId, Type>,
    pub return_type: Option<Type>,
    pub register_allocations: HashMap<VirtualId, RegisterId>,
}

impl ProcedureData {
    pub fn stack_variable_type(&self, stack_id: StackId) -> Option<Type> {
        self.stack_data.var_type(stack_id)
    }

    pub fn virtual_variable_type(&self, virt_id: VirtualId) -> Option<Type> {
        self.virtual_types.get(&virt_id).cloned()
    }

    pub fn register_allocation(&self, virt_id: VirtualId) -> Option<RegisterId> {
        self.register_allocations.get(&virt_id).copied()
    }

    pub fn set_register_allocation(&mut self, virt_id: VirtualId, reg_id: RegisterId) {
        self.register_allocations.insert(virt_id, reg_id);
    }

    pub fn acquire_new_virtual_variable(&mut self, typ: Type) -> Variable {
        // TODO Add a utility method on Peephole or something like that to simplify
        // inserting inflexion points (redundant movs) that take care of
        // propagating types.

        let virt_id = self.acquire_next_virtual_id();
        self.virtual_types.insert(virt_id, typ);
        Variable::Virtual(virt_id)
    }

    pub fn acquire_new_virtual_variable_for(&mut self, other: VirtualId) -> Variable {
        let typ = self
            .virtual_variable_type(other)
            .expect("other should have a type");
        self.acquire_new_virtual_variable(typ)
    }

    fn acquire_next_virtual_id(&mut self) -> VirtualId {
        self.highest_virtual_id = self.highest_virtual_id.checked_add(1).unwrap();
        self.highest_virtual_id
    }
}

#[derive(Debug, Default)]
pub struct StackData {
    pub vars: Vec<StackVar>,
    pub local_slots: Vec<StackSlot>, // Stack slots in local stack frame (local variables)
    pub param_slots: Vec<StackSlot>, // Stack slots from parent stack frame (parameters)
    pub callee_slots: BTreeMap<u32, BTreeMap<u32, StackSlot>>, // Stack slots in local stack frame for callee parameters
}

impl StackData {
    pub fn var_type(&self, stack_id: StackId) -> Option<Type> {
        let stack_var = *self.vars.get(stack_id as usize)?;
        let typ = match stack_var {
            StackVar::Local(idx) => self
                .local_slots
                .get(idx)
                .expect("local slot should exist if a stack var to it exists")
                .typ
                .clone(),
            StackVar::Param(idx) => self
                .param_slots
                .get(idx)
                .expect("param slot should exist if a stack var to it exists")
                .typ
                .clone(),
            StackVar::CalleeParam {
                callee_idx,
                param_idx,
            } => self
                .callee_slots
                .get(&callee_idx)
                .expect("stack call should exist if a stack var in it exists")
                .get(&param_idx)
                .expect("callee param slot should exist if a stack var to it exists")
                .typ
                .clone(),
        };
        Some(typ)
    }

    pub fn allocate_local_stack_slot(&mut self, typ: Type) -> StackId {
        let local_slot_idx = self.local_slots.len();
        self.local_slots.push(StackSlot { typ });

        let stack_id = self.vars.len();
        self.vars.push(StackVar::Local(local_slot_idx));

        stack_id.try_into().unwrap()
    }

    pub fn allocate_param_stack_slot(&mut self, typ: Type) -> StackId {
        let param_slot_idx = self.param_slots.len();
        self.param_slots.push(StackSlot { typ });

        let stack_id = self.vars.len();
        self.vars.push(StackVar::Param(param_slot_idx));

        stack_id.try_into().unwrap()
    }

    pub fn allocate_callee_param_stack_slot(
        &mut self,
        callee_idx: u32,
        param_idx: u32,
        typ: Type,
    ) -> StackId {
        self.callee_slots
            .entry(callee_idx)
            .or_default()
            .insert(param_idx, StackSlot { typ });

        let stack_id = self.vars.len();
        self.vars.push(StackVar::CalleeParam {
            callee_idx,
            param_idx,
        });

        stack_id.try_into().unwrap()
    }

    // FIXME handle alignment properly (you need to know what the stack frame is aligned to (from callconv probably))
    pub fn total_local_stack_size(&self, context: &Context) -> u64 {
        let local_size = self.total_local_stack_slots_size(context);
        let callees_size = self.total_callee_param_slots_size(context);
        local_size + callees_size
    }

    pub fn stack_var_memory_offset(&self, stack_id: StackId, context: &Context) -> i64 {
        // NOTE Currently works right (at lower address) to left (at higher address)
        // NOTE Assumes offsets for caller need to jump past a saved %rbp and the address pushed by the 'call', that is +16

        // Offset should be local to stack frame pointer.
        // Top_of_stack is (stack_frame_pointer - total_local_stack_size).
        // There are three cases:
        // 1. Local slot
        // 2. Param slot
        // 3. Callee param slot
        //
        // 1. Local slot offsets:
        //   are negative,
        //   go down from stack frame pointer,
        // 2. Param slot offsets:
        //   are positive,
        //   go up from stack frame pointer
        // 3. Callee param slot offsets:
        //   are negative,
        //   go up from top of stack,
        //   with higher cardinalities being closer to stack_frame_pointer.

        // FIXME handle alignment properly (you need to know what the stack frame is aligned to (from callconv probably))
        match self.vars[stack_id as usize] {
            StackVar::Local(slot_idx) => self.local_slot_offset(slot_idx, context),
            StackVar::Param(slot_idx) => self.param_offset(slot_idx, context),
            StackVar::CalleeParam {
                callee_idx,
                param_idx,
            } => self.callee_param_offset(callee_idx, param_idx, context),
        }
    }

    // FIXME Account for memory alignment
    fn total_local_stack_slots_size(&self, context: &Context) -> u64 {
        self.local_stack_slots_size_up_to_including(self.local_slots.len(), context)
    }

    // FIXME Account for memory alignment
    fn local_stack_slots_size_up_to_including(&self, slot_idx: usize, context: &Context) -> u64 {
        self.local_slots
            .iter()
            .enumerate()
            .take_while(|&(idx, _)| idx != slot_idx + 1)
            .map(|(_, slot)| slot.typ.sizeof(context))
            .sum()
    }

    // FIXME Does this needs to be aware of stack alignment requirements? idk.
    fn total_callee_param_slots_size(&self, context: &Context) -> u64 {
        self.callee_slots
            .iter()
            .map(|(&callee_idx, _)| self.callee_param_slot_size(callee_idx, context))
            .max()
            .unwrap_or(0)
    }

    // FIXME Account for memory alignment
    fn callee_param_slot_size(&self, callee_idx: u32, context: &Context) -> u64 {
        let callee = self
            .callee_slots
            .get(&callee_idx)
            .expect("if this panics, it's your fault");
        callee
            .iter()
            .map(|(_, slot)| slot.typ.sizeof(context))
            .sum()
    }

    // FIXME Account for memory alignment
    fn local_slot_offset(&self, slot_idx: usize, context: &Context) -> i64 {
        let size = self.local_stack_slots_size_up_to_including(slot_idx, context);
        let offset: i64 = size.try_into().unwrap();
        -offset
    }

    // FIXME Account for memory alignment
    fn param_offset(&self, slot_idx: usize, context: &Context) -> i64 {
        let caller_stack_frame_pointer_and_return_address = 2 * Type::Address.sizeof(context);
        let size_up_to_not_including: u64 = self
            .param_slots
            .iter()
            .enumerate()
            .take_while(|&(idx, _)| idx != slot_idx)
            .map(|(_, slot)| slot.typ.sizeof(context))
            .sum();

        let offset = caller_stack_frame_pointer_and_return_address + size_up_to_not_including;

        offset.try_into().unwrap()
    }

    // FIXME Account for memory alignment
    fn callee_param_offset(&self, call_idx: u32, param_idx: u32, context: &Context) -> i64 {
        let call_slot_size = self.total_callee_param_slots_size(context);

        let first_param_offset_from_frame =
            self.total_callee_param_slots_size(context) + call_slot_size;
        let first_param_offset_from_frame: i64 = first_param_offset_from_frame.try_into().unwrap();
        let first_param_offset_from_frame = -first_param_offset_from_frame;

        let param_offset_from_first_param: u64 = self
            .callee_slots
            .get(&call_idx)
            .unwrap()
            .iter()
            .take_while(|(&idx, _)| idx != param_idx)
            .map(|(_, slot)| slot.typ.sizeof(context))
            .sum();
        let param_offset_from_first_param: i64 = param_offset_from_first_param.try_into().unwrap();

        first_param_offset_from_frame + param_offset_from_first_param
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StackVar {
    Local(usize),
    Param(usize),
    CalleeParam { callee_idx: u32, param_idx: u32 },
}

#[derive(Debug)]
pub struct StackSlot {
    pub typ: Type,
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

    pub fn entry_parameters(&self) -> impl Iterator<Item = &Variable> {
        self.entry.parameters.iter()
    }

    pub fn entry_parameters_mut(&mut self) -> impl Iterator<Item = &mut Variable> {
        self.entry.parameters.iter_mut()
    }
}

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub parameters: Vec<Variable>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variable {
    Virtual(VirtualId),
    Register(RegisterId), // TODO remove register variant
    Stack(StackId),
    Data(DataId),
}

impl Variable {
    pub fn is_virtual(&self) -> bool {
        self.as_virtual().is_some()
    }

    pub fn as_virtual(&self) -> Option<VirtualId> {
        match self {
            Variable::Virtual(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_register(&self) -> Option<RegisterId> {
        match self {
            Variable::Register(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_stack(&self) -> Option<StackId> {
        match self {
            Variable::Stack(id) => Some(*id),
            _ => None,
        }
    }
}
