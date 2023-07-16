use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
};

use crate::{
    declarations::Declarations,
    instruction::{Instruction, Opcode, Operand},
    procedure::{Procedure, RegisterId, Variable, VirtualId},
    regalloc::InstructionConstraint,
    utils::Peephole,
};

use super::Isa;

// TODO Change "live range" to "live interval"

pub fn allocate_registers(proc: &mut Procedure, declarations: &Declarations) {
    minimize_block_arguments_live_ranges(proc);
    minimize_shared_src_dst_live_ranges(proc);
    minimize_pinned_live_ranges(proc);

    todo!("linear scan register allocation (that doesnt support pinned registers yet)");

    // OLD IMPLEMENTATION, DELETE WHEN BETTER IS MADE
    // let mut ranges = compute_live_ranges(proc);

    // let callconv = Isa::calling_convention(proc.signature.calling_convention.unwrap()).unwrap();
    // let mut reg_distributor = callconv.register_distributor();

    // // Pin correct registers to proc parameter live ranges (TODO and allocate stack space for excess, if needed)
    // for (i, param) in proc.blocks.entry_parameters().enumerate() {
    //     let var = param.variable.as_virtual().unwrap();
    //     let reg = *callconv.integer_parameter_registers.get(i).unwrap();
    //     ranges.assign_register_to_range_variable(reg, var);
    // }

    // // Pin correct registers to Call and Ret instrs
    // Peephole::peep_instructions(proc, |_, _, instr| match instr.opcode {
    //     Opcode::Call => {
    //         let target = instr
    //             .target
    //             .as_ref()
    //             .and_then(|t| t.as_procedure())
    //             .unwrap();
    //         let proc_decl = declarations.get_procedure(target).unwrap();
    //         let callconv_id = proc_decl.calling_convention.unwrap();
    //         let callee_callconv = Isa::calling_convention(callconv_id).unwrap();

    //         for (i, operand) in instr.operands().enumerate() {
    //             let var = operand.as_variable().unwrap().as_virtual().unwrap();
    //             let reg = *callee_callconv.integer_parameter_registers.get(i).unwrap();
    //             ranges.assign_register_to_range_variable(reg, var);
    //         }
    //         if let Some(dst) = &instr.dst {
    //             let var = dst.as_virtual().unwrap();
    //             let reg = callee_callconv.integer_return_register;
    //             ranges.assign_register_to_range_variable(reg, var);
    //         }
    //     }
    //     Opcode::Ret => {
    //         let operand = instr.operands().next().unwrap();
    //         let var = operand.as_variable().unwrap().as_virtual().unwrap();
    //         let reg = callconv.integer_return_register;
    //         ranges.assign_register_to_range_variable(reg, var);
    //     }
    //     _ => (),
    // });

    // // Allocate registers to all live ranges
    // for edge in ranges.iter_range_edges() {
    //     match edge {
    //         LiveRangeEdge::Start { range_id } => {
    //             let vars: Vec<String> = ranges
    //                 .range_variables
    //                 .get(&range_id)
    //                 .unwrap()
    //                 .iter()
    //                 .map(|v| format!("v{}", v))
    //                 .collect(); // DEBUG

    //             if let Some(reg) = ranges.range_register(range_id) {
    //                 eprintln!("DEBUG: Preassigned reg r{} to vars {:?}", reg, vars);
    //                 reg_distributor.take_register(reg);
    //             } else {
    //                 let reg = reg_distributor.take_scratch_register().unwrap();
    //                 eprintln!("DEBUG: Assigned reg r{} to vars {:?}", reg, vars);
    //                 ranges.assign_register_to_range(reg, range_id);
    //             }
    //         }
    //         LiveRangeEdge::End { range_id } => {
    //             let vars: Vec<String> = ranges
    //                 .range_variables
    //                 .get(&range_id)
    //                 .unwrap()
    //                 .iter()
    //                 .map(|v| format!("v{}", v))
    //                 .collect(); // DEBUG

    //             let salvaged_reg = ranges.range_register(range_id).unwrap();
    //             reg_distributor.give_register(salvaged_reg);
    //             eprintln!("DEBUG: Salvaged reg r{} from vars {:?}", salvaged_reg, vars);
    //         }
    //     }
    // }

    // // Change all virtual vars to their allocated registers.
    // for block in proc.blocks.iter_mut() {
    //     for param in &mut block.parameters {
    //         if let Some(var) = param.variable.as_virtual() {
    //             let reg = ranges.variable_register(var).unwrap();
    //             param.variable = Variable::Register(reg);
    //         }
    //     }

    //     for instr in &mut block.instructions {
    //         for operand in instr.operands_mut() {
    //             let Some(operand_var) = operand.as_variable() else { continue };
    //             let Some(var) = operand_var.as_virtual() else { continue };

    //             let reg = ranges.variable_register(var).unwrap();
    //             *operand = Operand::Var(Variable::Register(reg));
    //         }

    //         for operand in instr.condition_operands_mut() {
    //             let Some(operand_var) = operand.as_variable() else { continue };
    //             let Some(var) = operand_var.as_virtual() else { continue };

    //             let reg = ranges.variable_register(var).unwrap();
    //             *operand = Operand::Var(Variable::Register(reg));
    //         }

    //         if let Some(dst) = &mut instr.dst {
    //             if let Some(var) = dst.as_virtual() {
    //                 let reg = ranges.variable_register(var).unwrap();
    //                 *dst = Variable::Register(reg);
    //             }
    //         }
    //     }
    // }
}

fn minimize_block_arguments_live_ranges(proc: &mut Procedure) {
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
    });
}

fn minimize_shared_src_dst_live_ranges(proc: &mut Procedure) {
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

fn minimize_pinned_live_ranges(proc: &mut Procedure) {
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

                // Avoid emitting multiple moves for the save var if it's used multiple times.
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

fn compute_live_ranges(proc: &Procedure) -> LiveRanges {
    let mut ranges = LiveRanges::default();
    let mut instr_idx = 0;

    for block in proc.blocks.iter() {
        // Track virtual block parameters
        for param in &block.parameters {
            let Some(virt_var_id) = param.variable.as_virtual() else { continue };
            ranges.track_variable(virt_var_id, Position::Pre(instr_idx));
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
                ranges.track_variable(virt_var_id, Position::Pre(instr_idx));
            }

            // Track virtual cond operands
            for operand in instr.condition_operands() {
                let Some(operand_var) = operand.as_variable() else { continue };
                let Some(virt_var_id) = operand_var.as_virtual() else { continue };
                ranges.track_variable(virt_var_id, Position::Pre(instr_idx));
            }

            // Track virtual dst
            if let Some(dst) = &instr.dst {
                if first_operand_is_also_dst {
                    let first_operand = instr.operands().next().unwrap();
                    let first_operand_virt_var_id =
                        first_operand.as_variable().unwrap().as_virtual().unwrap();
                    let dst_virt_var_id = dst.as_virtual().unwrap();
                    ranges.tie_tracked_var_with_other(first_operand_virt_var_id, dst_virt_var_id);
                }

                if let Some(dst_virt_var_id) = dst.as_virtual() {
                    ranges.track_variable(dst_virt_var_id, Position::Post(instr_idx));
                }
            }

            instr_idx += 1;
        }
    }

    ranges
}

#[derive(Debug, Default)]
struct LiveRanges {
    ranges: Vec<LiveRange>,
    range_variables: HashMap<RangeId, HashSet<VirtualId>>,
    range_register: HashMap<RangeId, RegisterId>,
    variable_range: HashMap<VirtualId, RangeId>,
}

type RangeId = usize;

impl LiveRanges {
    pub fn track_variable(&mut self, var: VirtualId, position: Position) {
        if !self.variable_range.contains_key(&var) {
            self.make_new_range_for_var(var, position);
        }

        let range_id = *self.variable_range.get(&var).unwrap();
        self.ranges[range_id].end_position = position;
    }

    pub fn tie_tracked_var_with_other(&mut self, var: VirtualId, other: VirtualId) {
        let range_id = *self
            .variable_range
            .get(&var)
            .expect("var should already be tracked");
        self.variable_range
            .insert(other, range_id)
            .ok_or(())
            .expect_err("other should not be already tracked");
        self.range_variables
            .get_mut(&range_id)
            .unwrap()
            .insert(other);
    }

    // pub fn range_variables(&self, range_id: usize) -> impl Iterator<Item = VirtualId> + '_ {
    //     let vars = self.range_variables.get(&range_id).unwrap();
    //     vars.iter().copied()
    // }

    pub fn assign_register_to_range(&mut self, reg: RegisterId, range_id: RangeId) {
        self.range_register.insert(range_id, reg);
    }

    pub fn assign_register_to_range_variable(&mut self, reg: RegisterId, var: VirtualId) {
        let range_id = *self.variable_range.get(&var).unwrap();
        self.assign_register_to_range(reg, range_id);
    }

    pub fn range_register(&self, range_id: RangeId) -> Option<RegisterId> {
        self.range_register.get(&range_id).copied()
    }

    pub fn _variable_range(&self, var: VirtualId) -> Option<RangeId> {
        self.variable_range.get(&var).copied()
    }

    pub fn variable_register(&self, var: VirtualId) -> Option<RegisterId> {
        let range_id = *self.variable_range.get(&var).unwrap();
        self.range_register(range_id)
    }

    pub fn iter_range_edges(&self) -> impl Iterator<Item = LiveRangeEdge> {
        let mut edges = BTreeMap::<Position, Vec<LiveRangeEdge>>::new();
        for (range_id, range) in self.ranges.iter().enumerate() {
            edges
                .entry(range.start_position)
                .or_default()
                .push(LiveRangeEdge::Start { range_id });
            edges
                .entry(range.end_position)
                .or_default()
                .push(LiveRangeEdge::End { range_id });
        }

        edges.into_iter().flat_map(|(_, v)| v.into_iter())
    }

    fn make_new_range_for_var(&mut self, var: VirtualId, start_position: Position) {
        let idx = self.ranges.len();
        self.ranges.push(LiveRange {
            start_position,
            end_position: start_position,
        });
        self.range_variables.insert(idx, HashSet::from([var]));
        self.variable_range.insert(var, idx);
    }
}

#[derive(Debug)]
struct LiveRange {
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

#[derive(Debug, Clone, Copy)]
enum LiveRangeEdge {
    Start { range_id: usize },
    End { range_id: usize },
}
