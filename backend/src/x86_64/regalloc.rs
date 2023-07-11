use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
};

use crate::{
    instruction::Operand,
    procedure::{Procedure, Variable, VirtualId},
    regalloc::InstructionConstraint,
};

use super::Isa;

pub fn allocate_registers(proc: &mut Procedure) {
    let ranges = compute_live_ranges(proc);

    let mut available_registers = Isa::caller_saved_ids().to_vec();
    available_registers.reverse();

    let mut virtual_to_register: HashMap<Variable, Variable> = HashMap::new();

    for edge in ranges.iter_range_edges() {
        match edge {
            LiveRangeEdge::Start { range_id } => {
                let reg = available_registers.pop().unwrap();
                let reg_var = Variable::Register(reg);
                for var_id in ranges.range_variables(range_id) {
                    virtual_to_register.insert(Variable::Virtual(var_id), reg_var);
                }
            }
            LiveRangeEdge::End { range_id } => {
                let mut salvaged_reg = None;
                for var_id in ranges.range_variables(range_id) {
                    let reg_var = virtual_to_register.get(&Variable::Virtual(var_id)).unwrap();
                    salvaged_reg = Some(reg_var.as_register().unwrap());
                }
                available_registers.push(salvaged_reg.unwrap());
            }
        }
    }

    for block in proc.blocks.iter_mut() {
        for instr in &mut block.instructions {
            for operand in instr.operands_mut() {
                let Some(operand_var) = operand.as_variable() else { continue };
                if !operand_var.is_virtual() {
                    continue;
                }

                let register = *virtual_to_register.get(&operand_var).unwrap();
                *operand = Operand::Var(register);
            }

            for operand in instr.condition_operands_mut() {
                let Some(operand_var) = operand.as_variable() else { continue };
                if !operand_var.is_virtual() {
                    continue;
                }

                let register = *virtual_to_register.get(&operand_var).unwrap();
                *operand = Operand::Var(register);
            }

            if let Some(dst) = &mut instr.dst {
                if dst.is_virtual() {
                    let register = *virtual_to_register.get(&dst).unwrap();
                    *dst = register;
                }
            }
        }
    }
}

fn compute_live_ranges(proc: &Procedure) -> LiveRanges {
    let mut ranges = LiveRanges::default();
    let mut instr_idx = 0;

    // Track virtual proc parameters
    for param in &proc.signature.parameters {
        let Some(var) = param.variable.as_virtual() else { continue };
        ranges.track_variable(var, Position::Pre(instr_idx));
    }

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
    range_variables: HashMap<usize, HashSet<VirtualId>>,
    variable_range: HashMap<VirtualId, usize>,
}

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

    pub fn range_variables(&self, range_id: usize) -> impl Iterator<Item = VirtualId> + '_ {
        let vars = self.range_variables.get(&range_id).unwrap();
        vars.iter().copied()
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
