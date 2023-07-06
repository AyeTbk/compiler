use crate::{
    instruction::Instruction,
    module::{Block, Procedure, ProcedureData},
};

pub struct Peephole<'a> {
    pub inserter: InstructionInserter,
    pub data: &'a mut ProcedureData,
}

impl<'a> Peephole<'a> {
    pub fn peep_instructions(
        proc: &'a mut Procedure,
        mut peeper: impl FnMut(&mut Peephole, usize, &mut Instruction),
    ) {
        let blocks = &mut proc.blocks;
        let mut peephole = Self {
            inserter: Default::default(),
            data: &mut proc.data,
        };
        for block in blocks.iter_mut() {
            for (i, instr) in block.instructions.iter_mut().enumerate() {
                peeper(&mut peephole, i, instr);
            }
            peephole.inserter.apply(block);
            peephole.inserter = Default::default();
        }
    }

    pub fn peep_blocks(
        proc: &'a mut Procedure,
        mut peeper: impl FnMut(&mut Peephole, usize, &mut Block),
    ) {
        let blocks = &mut proc.blocks;
        let mut peephole = Self {
            inserter: Default::default(),
            data: &mut proc.data,
        };
        for (i, block) in blocks.iter_mut().enumerate() {
            peeper(&mut peephole, i, block);

            peephole.inserter.apply(block);
            peephole.inserter = Default::default();
        }
    }

    pub fn insert_before(&mut self, idx: usize, instr: Instruction) {
        self.inserter.insert_before(idx, instr);
    }

    pub fn insert_after(&mut self, idx: usize, instr: Instruction) {
        self.inserter.insert_after(idx, instr);
    }
}

// TODO make this a more "generic" instruction editing utility, with instruction remove, replace, etc.
#[derive(Debug, Default)]
pub struct InstructionInserter {
    inserts: Vec<(usize, Instruction)>,
}

impl InstructionInserter {
    pub fn insert_before(&mut self, idx: usize, instr: Instruction) {
        self.inserts.push((idx, instr));
    }

    pub fn insert_after(&mut self, idx: usize, instr: Instruction) {
        let after_idx = idx.checked_add(1).unwrap();
        self.inserts.push((after_idx, instr));
    }

    pub fn apply(mut self, block: &mut Block) {
        self.inserts.sort_by_key(|(idx, _)| *idx);
        self.inserts.reverse();
        for (idx, instr) in self.inserts {
            block.instructions.insert(idx, instr);
        }
    }
}
