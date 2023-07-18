use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use crate::{
    declarations::Declarations,
    instruction::{Instruction, Opcode, Operand},
    procedure::{Procedure, RegisterId, StackId, Variable, VirtualId},
    regalloc::InstructionConstraint,
    utils::Peephole,
};

use super::Isa;

pub fn allocate_registers(proc: &mut Procedure, decls: &Declarations) {
    minimize_block_arguments_live_intervals(proc);
    minimize_shared_src_dst_live_intervals(proc);
    minimize_pinned_live_intervals(proc);

    let mut intervals = compute_live_intervals(proc);

    pin_live_intervals_callconv(&mut intervals, proc, decls);
    pin_live_intervals_block_parameters(&mut intervals, proc);

    let allocs = linear_scan_allocation(proc, &mut intervals);

    apply_allocations(proc, &allocs);
}

fn pin_live_intervals_callconv(
    intervals: &mut LiveIntervals,
    proc: &mut Procedure,
    decls: &Declarations,
) {
    // pin entry block parameters to proc callconv.
    let proc_callconv =
        Isa::calling_convention(proc.signature.calling_convention.unwrap()).unwrap();
    for (i, param) in proc.blocks.entry_parameters().enumerate() {
        // FIXME handle need for spill.
        let reg = proc_callconv.integer_parameter_registers[i];
        let var = param.variable.as_virtual().unwrap();
        intervals.set_variable_pinned_allocation(var, Allocation::Register(reg));
    }

    // for every instrs:
    //   on Ret: pin operand to proc callconv.
    //   on Call: pin operands and dst to target proc / callee callconv.
    Peephole::peep_instructions(proc, |_, _, instr| {
        if instr.opcode == Opcode::Ret {
            // Multiple return values not supported right now.
            assert!(instr.operands().count() <= 1);
            for operand in instr.operands() {
                let reg = proc_callconv.integer_return_register;
                let var = operand.as_variable().unwrap().as_virtual().unwrap();
                intervals.set_variable_pinned_allocation(var, Allocation::Register(reg));
            }
        } else if instr.opcode == Opcode::Call {
            let callee_name = instr.target.as_ref().unwrap().as_procedure().unwrap();
            let callee_decl = decls.get_procedure(callee_name).unwrap();
            let calleeconv_id = callee_decl.calling_convention.unwrap();
            let calleeconv = Isa::calling_convention(calleeconv_id).unwrap();

            for (i, operand) in instr.operands().enumerate() {
                // FIXME handle need for spill.
                let reg = calleeconv.integer_parameter_registers[i];
                let var = operand.as_variable().unwrap().as_virtual().unwrap();
                intervals.set_variable_pinned_allocation(var, Allocation::Register(reg));
            }
            if let Some(dst) = &instr.dst {
                let reg = calleeconv.integer_return_register;
                let var = dst.as_virtual().unwrap();
                intervals.set_variable_pinned_allocation(var, Allocation::Register(reg));
            }
        }
    });
}

fn pin_live_intervals_block_parameters(intervals: &mut LiveIntervals, proc: &mut Procedure) {
    let mut block_parameters_pins: HashMap<String, Vec<RegisterId>> = Default::default();

    // Pin block parameters to registers.
    {
        let callconv = Isa::calling_convention(proc.signature.calling_convention.unwrap()).unwrap();
        for block in &proc.blocks.others {
            let mut regs = callconv.scratch_registers.clone();
            let mut allocated_regs = Vec::new();
            for param in &block.parameters {
                let var = param.variable.as_virtual().unwrap();
                let reg = regs
                    .pop()
                    .expect("if this panics, improve this allocation scheme, tyvm");
                intervals.set_variable_pinned_allocation(var, Allocation::Register(reg));
                allocated_regs.push(reg);
            }
            block_parameters_pins.insert(block.name.clone(), allocated_regs);
        }
    }

    // Pin Jump operands accordingly.
    Peephole::peep_instructions(proc, |_, _, instr| {
        if instr.opcode != Opcode::Jump {
            return;
        }

        let target_block = instr.target.as_ref().unwrap().as_block().unwrap();
        let parameters_pins = block_parameters_pins.get(target_block).unwrap();
        assert_eq!(parameters_pins.len(), instr.operands().count());
        for (&operand, &pin) in instr.operands().zip(parameters_pins.iter()) {
            let var = operand.as_variable().unwrap().as_virtual().unwrap();
            intervals.set_variable_pinned_allocation(var, Allocation::Register(pin));
        }
    });
}

fn apply_allocations(proc: &mut Procedure, allocs: &Allocations) {
    let mut allocated_stack_vars: HashMap<VirtualId, StackId> = Default::default();
    for (&virt_id, alloc) in &allocs.allocations {
        if matches!(alloc, Allocation::Spilled) {
            let stack_id = proc.data.stack_data.allocate_local_stack_slot();
            allocated_stack_vars.insert(virt_id, stack_id);
        }
    }

    let do_alloc = |variable: &mut Variable| {
        let var = variable.as_virtual().unwrap();
        let alloc = allocs.allocations.get(&var).unwrap();
        match alloc {
            Allocation::Register(reg) => {
                *variable = Variable::Register(*reg);
            }
            Allocation::Spilled => {
                let stack_id = allocated_stack_vars.get(&var).unwrap();
                *variable = Variable::Stack(*stack_id)
            }
        }
    };

    Peephole::peep_blocks(proc, |_, _, block| {
        for param in &mut block.parameters {
            do_alloc(&mut param.variable);
        }

        for instr in block.instructions.iter_mut() {
            for operand in instr.operands_mut() {
                if let Some(variable) = operand.as_variable_mut() {
                    do_alloc(variable);
                }
            }
            for cond_operand in instr.condition_operands_mut() {
                if let Some(variable) = cond_operand.as_variable_mut() {
                    do_alloc(variable);
                }
            }
            if let Some(dst) = &mut instr.dst {
                do_alloc(dst);
            }
        }
    });

    correct_move_load_stores(proc);
}

fn correct_move_load_stores(proc: &mut Procedure) {
    // Corrections:
    // sX = move rY   =>   change opcode to Store.
    // rX = move sY   =>   change opcode to Load.

    // TODO: add this too vvv
    // X = move X   =>   remove instr.

    Peephole::peep_instructions(proc, |_, _, instr| {
        if instr.opcode != Opcode::Move {
            return;
        }

        let src = instr.operands().next().unwrap();
        let dst = instr.dst.unwrap();

        use Variable::*;
        match (dst, src.as_variable()) {
            (Stack(_), Some(Register(_))) => instr.opcode = Opcode::Store,
            (Register(_), Some(Stack(_))) => instr.opcode = Opcode::Load,
            _ => (),
        }
    });
}

// Based on "Linear Scan Register Allocation" by Poletto & Sarkar
// https://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
fn linear_scan_allocation(proc: &Procedure, intervals: &mut LiveIntervals) -> Allocations {
    fn expire_old_intervals(
        current_interval_id: IntervalId,
        intervals: &LiveIntervals,
        active_intervals: &mut Vec<IntervalId>,
        free_registers: &mut Vec<RegisterId>,
    ) {
        let current_position = intervals.start_position(current_interval_id);
        let mut new_active_intervals = Vec::new();
        for &interval_id in active_intervals.iter() {
            let interval_end = intervals.end_position(interval_id);
            let interval_is_no_longer_active = interval_end < current_position;
            if interval_is_no_longer_active {
                let reg = intervals
                    .interval_allocation(interval_id)
                    .unwrap()
                    .as_register()
                    .unwrap();
                free_registers.push(reg)
            } else {
                new_active_intervals.push(interval_id);
            }
        }
        *active_intervals = new_active_intervals;
    }

    fn spill_at_interval(
        current_interval_id: IntervalId,
        intervals: &mut LiveIntervals,
        active_intervals: &mut Vec<IntervalId>,
    ) {
        // Make sure to select a candidate that isnt pinned to a register!
        let spill_candidate_id = active_intervals
            .iter()
            .copied()
            .rfind(|&interval_id| intervals.interval_pinned_allocation(interval_id).is_none());
        if spill_candidate_id.is_none() {
            if active_intervals.len() == 0 {
                panic!("spilling shouldn't be needed if there aren't any active live intervals");
            } else {
                panic!("impossible to spill, all active intervals are pinned");
            }
        }
        let spill_candidate_id = spill_candidate_id.unwrap();

        // Heuristic: spill the interval that ends last (/ lasts longest from this point onward)
        if intervals.end_position(spill_candidate_id) > intervals.end_position(current_interval_id)
        {
            // Reallocate the register spill_candidate had been assigned to the current_interval.
            // Allocate spill_candidate as spilled.
            let alloc = intervals.interval_allocation(spill_candidate_id).unwrap();
            intervals.set_interval_allocation(spill_candidate_id, Allocation::Spilled);
            intervals.set_interval_allocation(current_interval_id, alloc);

            // Remove spill_candidate from active.
            active_intervals.pop();

            // Add current_interval to active, sorted by increasing end point.
            add_interval_to_active_list(current_interval_id, intervals, active_intervals);
        } else {
            // Allocate current_interval as spilled.
            intervals.set_interval_allocation(current_interval_id, Allocation::Spilled);
        }
    }

    fn add_interval_to_active_list(
        interval_id: IntervalId,
        intervals: &LiveIntervals,
        active_intervals: &mut Vec<IntervalId>,
    ) {
        // Add the live interval to the active list, sorted by increasing end point.
        let interval_end = intervals.end_position(interval_id);
        let insert_idx = active_intervals
            .iter()
            .enumerate()
            .map(|(i, &id)| (i, intervals.end_position(id)))
            .find(|(_, end)| *end > interval_end)
            .map(|(i, _)| i)
            .unwrap_or(active_intervals.len());
        active_intervals.insert(insert_idx, interval_id);
    }

    let callconv = Isa::calling_convention(proc.signature.calling_convention.unwrap()).unwrap();
    let mut free_registers = callconv.scratch_registers.clone();
    free_registers.reverse();
    let mut active_intervals: Vec<IntervalId> = Default::default();

    for interval_id in 0..(intervals.intervals.len() as IntervalId) {
        expire_old_intervals(
            interval_id,
            &intervals,
            &mut active_intervals,
            &mut free_registers,
        );

        let maybe_alloc = intervals.interval_pinned_allocation(interval_id);
        if let Some(Allocation::Register(reg)) = maybe_alloc {
            if !free_registers.contains(&reg) {
                // find who has reg in active_intervals.
                let (i, culprit) = active_intervals
                    .iter()
                    .copied()
                    .enumerate()
                    .find(|&(_, id)| {
                        let Some(Allocation::Register(r)) = intervals.interval_allocation(id) else {
                        return false
                    };
                        r == reg
                    })
                    .expect("a culprit should be found, otherwise pinned reg doesn't exist");
                // spill them.
                intervals.set_interval_allocation(culprit, Allocation::Spilled);
                active_intervals.remove(i);
            } else {
                // Remove reg from available registers
                free_registers = free_registers.into_iter().filter(|&r| r != reg).collect();
            }
            intervals.set_interval_allocation(interval_id, Allocation::Register(reg));
            add_interval_to_active_list(interval_id, &intervals, &mut active_intervals);
        } else if let Some(Allocation::Spilled) = maybe_alloc {
            unimplemented!();
            // spill curr_interval.
        } else if active_intervals.len() == free_registers.len() {
            spill_at_interval(interval_id, intervals, &mut active_intervals);
        } else {
            let reg = free_registers.pop().unwrap();
            intervals.set_interval_allocation(interval_id, Allocation::Register(reg));
            add_interval_to_active_list(interval_id, &intervals, &mut active_intervals);
        }
    }

    let mut allocs = Allocations::default();
    for interval_id in 0..(intervals.intervals.len() as IntervalId) {
        let allocation = intervals.interval_allocation(interval_id).unwrap();
        for var in intervals.interval_variables(interval_id) {
            assert!(!allocs.allocations.contains_key(&var));
            allocs.allocations.insert(var, allocation);
        }
    }
    allocs
}

#[derive(Debug, Default)]
struct Allocations {
    allocations: HashMap<VirtualId, Allocation>,
}

#[derive(Debug, Clone, Copy)]
enum Allocation {
    Register(RegisterId),
    Spilled,
}

impl Allocation {
    pub fn as_register(&self) -> Option<RegisterId> {
        match self {
            Allocation::Register(reg) => Some(*reg),
            _ => None,
        }
    }
}

fn minimize_block_arguments_live_intervals(proc: &mut Procedure) {
    // All block arguments of all jumps to that block need to use the same registers.
    // To simplify register allocation, minimize the live range of block arguments.

    Peephole::peep_instructions(proc, |ph, i, instr| {
        if !matches!(instr.opcode, Opcode::Jump) {
            return;
        }

        for operand in instr.operands_mut() {
            let new_var = ph.proc_data.acquire_new_virtual_variable();
            ph.insert_before(i, Instruction::mov(new_var, *operand));
            *operand = Operand::Var(new_var);
        }

        for cond_operand in instr.condition_operands_mut() {
            if cond_operand.as_variable().is_some() {
                let new_var = ph.proc_data.acquire_new_virtual_variable();
                ph.insert_before(i, Instruction::mov(new_var, *cond_operand));
                *cond_operand = Operand::Var(new_var);
            }
        }
    });
}

fn minimize_shared_src_dst_live_intervals(proc: &mut Procedure) {
    // For some ISAs (primarily x86), some instructions tie one of the src operand
    // and the dst such that they are the same register. To simplify register
    // allocation, split the ranges of the variables of instrs with such a
    // constraint.
    Peephole::peep_instructions(proc, |ph, i, instr| {
        if !matches!(
            Isa::instruction_constraint(instr.opcode),
            Some(InstructionConstraint::FirstOperandIsAlsoDestination)
        ) {
            return;
        }

        let first_operand = instr.operands_mut().next().unwrap();
        let new_var = ph.proc_data.acquire_new_virtual_variable();
        ph.insert_before(i, Instruction::mov(new_var, *first_operand));
        *first_operand = new_var.into();
    });
}

fn minimize_pinned_live_intervals(proc: &mut Procedure) {
    // Live ranges might need to be pinned to certain registers, mostly because of
    // calling conventions (so, mainly proc parameters and Call and Ret instrs).
    // For every variable that needs to be pinned, introduce a Move to split up its
    // live range and minimize the (new) live range that need to be pinned to a
    // register, that is, insert a Move before the instr for its every operands and
    // a Move after for the dst, and a Move at the start of the proc for each parameter
    // that would be pinned to a register.
    // This should greatly simplify register allocation at the cost of increased
    // register pressure, which could then be improved by a subsequent register
    // coalescing pass.
    //
    // This approach is in contrast with instead keeping the ranges as they are,
    // then detecting conflicts introduced by ranges being pinned to multiple
    // registers and/or ranges having to share the same register at the same time,
    // and then resolving these conflicts, which sounds more complicated to me.

    let callconv = Isa::calling_convention(proc.signature.calling_convention.unwrap()).unwrap();

    let mut new_var_map: HashMap<Variable, Variable> = Default::default();
    Peephole::peep_blocks(proc, |ph, block_i, block| {
        if block_i == 0 {
            for (i, param) in block.parameters.iter_mut().enumerate() {
                if i < callconv.integer_parameter_registers.len() {
                    let new_var = ph.proc_data.acquire_new_virtual_variable();
                    new_var_map.insert(param.variable, new_var);
                    ph.insert_before(0, Instruction::mov(new_var, param.variable));
                } else {
                    unimplemented!("stack?");
                    // answer: probably not, I think you should ignore callconv here, and
                    //just add Moves to operands and dst.
                }
            }
        }

        for (i, instr) in block.instructions.iter_mut().enumerate() {
            // Update all src operands, cond operands and dst.
            for operand in instr.operands_mut() {
                let Some(&operand_var) = operand.as_variable() else { continue };
                let Some(&new_var) = new_var_map.get(&operand_var) else { continue };
                *operand = new_var.into();
            }
            for operand in instr.condition_operands_mut() {
                let Some(&operand_var) = operand.as_variable() else { continue };
                let Some(&new_var) = new_var_map.get(&operand_var) else { continue };
                *operand = new_var.into();
            }
            for dst_var in &mut instr.dst {
                let Some(&new_var) = new_var_map.get(dst_var) else { continue };
                *dst_var = new_var;
            }

            // Insert Moves correctly for Call and Ret instrs.
            if matches!(instr.opcode, Opcode::Call | Opcode::Ret) {
                // No such thing as conditional Call or Ret
                assert_eq!(instr.condition_operands().count(), 0);

                // Avoid emitting multiple moves for the same var if it's used multiple times.
                let unique_operands: HashSet<Operand> =
                    HashSet::from_iter(instr.operands().copied());
                for operand in unique_operands {
                    let Some(&operand_var) = operand.as_variable() else { continue };
                    let new_var = ph.proc_data.acquire_new_virtual_variable();
                    new_var_map.insert(operand_var, new_var);
                }

                for operand in instr.operands_mut() {
                    let new_var = match operand {
                        Operand::Var(operand_var) => {
                            let Some(&new_var) = new_var_map.get(&operand_var) else { continue };
                            new_var
                        }
                        Operand::Imm(_) => {
                            let new_var = ph.proc_data.acquire_new_virtual_variable();
                            new_var
                        }
                    };
                    ph.insert_before(i, Instruction::mov(new_var, *operand));
                    *operand = new_var.into();
                }
                for &dst_var in &instr.dst {
                    let new_var = ph.proc_data.acquire_new_virtual_variable();
                    new_var_map.insert(dst_var, new_var);
                    ph.insert_after(i, Instruction::mov(new_var, dst_var));
                }
            }
        }
    });
}

fn compute_live_intervals(proc: &Procedure) -> LiveIntervals {
    // The resulting interval list is ordered by the intervals start position, which
    // is required by the linear scan register allocator.

    let mut intervals = LiveIntervals::default();
    let mut instr_idx = 0;

    for block in proc.blocks.iter() {
        // Track virtual block parameters
        for param in &block.parameters {
            let Some(virt_var_id) = param.variable.as_virtual() else { continue };
            intervals.track_variable(virt_var_id, Position::Pre(instr_idx));
        }

        for instr in block.instructions.iter() {
            let constraint = Isa::instruction_constraint(instr.opcode);
            let first_operand_is_also_dst = matches!(
                constraint,
                Some(InstructionConstraint::FirstOperandIsAlsoDestination)
            );

            // Track virtual operands
            for operand in instr.operands() {
                let Some(operand_var) = operand.as_variable() else { continue };
                let Some(virt_var_id) = operand_var.as_virtual() else { continue };
                intervals.track_variable(virt_var_id, Position::Pre(instr_idx));
            }

            // Track virtual cond operands
            for operand in instr.condition_operands() {
                let Some(operand_var) = operand.as_variable() else { continue };
                let Some(virt_var_id) = operand_var.as_virtual() else { continue };
                intervals.track_variable(virt_var_id, Position::Pre(instr_idx));
            }

            // Track virtual dst
            if let Some(dst) = &instr.dst {
                if first_operand_is_also_dst {
                    let first_operand = instr.operands().next().unwrap();
                    let first_operand_virt_var_id =
                        first_operand.as_variable().unwrap().as_virtual().unwrap();
                    let dst_virt_var_id = dst.as_virtual().unwrap();
                    intervals
                        .tie_tracked_var_with_other(first_operand_virt_var_id, dst_virt_var_id);
                }

                if let Some(dst_virt_var_id) = dst.as_virtual() {
                    intervals.track_variable(dst_virt_var_id, Position::Post(instr_idx));
                }
            }

            instr_idx += 1;
        }
    }

    intervals
}

#[derive(Debug, Default)]
struct LiveIntervals {
    intervals: Vec<LiveInterval>,
    interval_variables: HashMap<IntervalId, HashSet<VirtualId>>,
    interval_allocs: HashMap<IntervalId, Allocation>,
    interval_pinned_allocs: HashMap<IntervalId, Allocation>,
    variable_interval: HashMap<VirtualId, IntervalId>,
}

type IntervalId = usize;

impl LiveIntervals {
    pub fn track_variable(&mut self, var: VirtualId, position: Position) {
        if !self.variable_interval.contains_key(&var) {
            self.make_new_interval_for_var(var, position);
        }

        let interval_id = *self.variable_interval.get(&var).unwrap();
        self.intervals[interval_id].end_position = position;
    }

    pub fn tie_tracked_var_with_other(&mut self, var: VirtualId, other: VirtualId) {
        let interval_id = *self
            .variable_interval
            .get(&var)
            .expect("var should already be tracked");
        self.variable_interval
            .insert(other, interval_id)
            .ok_or(())
            .expect_err("other should not be already tracked");
        self.interval_variables
            .get_mut(&interval_id)
            .unwrap()
            .insert(other);
    }

    pub fn start_position(&self, interval_id: IntervalId) -> Position {
        self.intervals[interval_id].start_position
    }

    pub fn end_position(&self, interval_id: IntervalId) -> Position {
        self.intervals[interval_id].end_position
    }

    pub fn interval_allocation(&self, interval_id: IntervalId) -> Option<Allocation> {
        self.interval_allocs.get(&interval_id).copied()
    }

    pub fn set_interval_allocation(&mut self, interval_id: IntervalId, alloc: Allocation) {
        self.interval_allocs.insert(interval_id, alloc);
    }

    pub fn interval_pinned_allocation(&self, interval_id: IntervalId) -> Option<Allocation> {
        self.interval_pinned_allocs.get(&interval_id).copied()
    }

    pub fn set_interval_pinned_allocation(&mut self, interval_id: IntervalId, alloc: Allocation) {
        self.interval_pinned_allocs.insert(interval_id, alloc);
    }

    pub fn set_variable_pinned_allocation(&mut self, var: VirtualId, alloc: Allocation) {
        let interval_id = *self.variable_interval.get(&var).unwrap();
        assert!(
            self.interval_pinned_allocation(interval_id).is_none(),
            "variable should not be pinned to more than one allocation"
        );
        self.set_interval_pinned_allocation(interval_id, alloc);
    }

    pub fn interval_variables(
        &self,
        interval_id: IntervalId,
    ) -> impl Iterator<Item = VirtualId> + '_ {
        self.interval_variables
            .get(&interval_id)
            .unwrap()
            .iter()
            .copied()
    }

    fn make_new_interval_for_var(&mut self, var: VirtualId, start_position: Position) {
        let idx = self.intervals.len();
        self.intervals.push(LiveInterval {
            start_position,
            end_position: start_position,
        });
        self.interval_variables.insert(idx, HashSet::from([var]));
        self.variable_interval.insert(var, idx);
    }
}

#[derive(Debug)]
struct LiveInterval {
    start_position: Position,
    end_position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Position {
    Pre(usize),  // Right before instr
    Post(usize), // Right after instr
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Position::*;
        match (self, other) {
            (Pre(this), Post(othr)) if this == othr => Some(Ordering::Less),
            (Post(this), Pre(othr)) if this == othr => Some(Ordering::Greater),
            (Pre(this), Post(othr))
            | (Post(this), Pre(othr))
            | (Pre(this), Pre(othr))
            | (Post(this), Post(othr)) => this.partial_cmp(&othr),
        }
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
