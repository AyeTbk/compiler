use crate::{
    context::Context,
    instruction::{Condition as IrCondition, Opcode, Operand as IrOperand, Target as IrTarget},
    module::Module,
    procedure::{Procedure as IrProcedure, Variable},
    x86_64::{
        mir::instruction::{Offset, SizedRegister},
        Register,
    },
};

pub mod instruction;
use self::instruction::{Condition, Immediate, JumpTarget, Memory, Operand, Operands, SizedMemory};

use super::Size;

pub type BlockId = usize;

pub struct MirModule {
    pub procedures: Vec<MirProcedure>,
}

pub struct MirProcedure {
    pub blocks: Vec<MirBlock>,
}

pub struct MirBlock {
    pub instrs: Vec<MirInstruction>,
}

#[derive(Debug)]
pub struct MirInstruction {
    pub operands: Operands,
    pub condition: Option<Condition>,
    pub signed: bool,
}

pub fn make_mir(module: &Module, context: &Context) -> MirModule {
    let mut mir_procs = Vec::new();
    for proc in &module.procedures {
        let mut mir_blocks = Vec::new();
        for block in proc.blocks.iter() {
            let mut instrs = Vec::new();
            for instr in &block.instructions {
                let mut condition = None;
                let operands = match instr.opcode {
                    Opcode::Call => Operands::ZeroOperand,
                    Opcode::Jump => {
                        let jump_target =
                            make_mir_jump_target(proc, instr.target.as_ref().unwrap())
                                .expect("invalid jump target");

                        if let Some(ir_cond) = instr.cond.as_ref() {
                            condition = Some(make_mir_condition(module, context, proc, ir_cond));
                        }
                        Operands::JumpTarget(jump_target)
                    }
                    Opcode::Move => {
                        let op1 = instr.dst.map(|dst| {
                            make_mir_operand(module, context, proc, &IrOperand::Var(dst))
                        });
                        let op2 = instr
                            .operands()
                            .next()
                            .map(|op| make_mir_operand(module, context, proc, op));
                        combine_mir_operands(op1, op2).expect("invalid condition operands")
                    }
                    _ => {
                        let mut o = instr.operands();
                        let op1 = o
                            .next()
                            .map(|op| make_mir_operand(module, context, proc, op));
                        let op2 = o
                            .next()
                            .map(|op| make_mir_operand(module, context, proc, op));
                        combine_mir_operands(op1, op2).expect("invalid condition operands")
                    }
                };
                instrs.push(MirInstruction {
                    operands,
                    condition,
                    signed: false,
                });
            }
            let mir_block = MirBlock { instrs };
            mir_blocks.push(mir_block);
        }
        let mir_proc = MirProcedure { blocks: mir_blocks };
        mir_procs.push(mir_proc);
    }

    MirModule {
        procedures: mir_procs,
    }
}

fn make_mir_condition(
    module: &Module,
    context: &Context,
    proc: &IrProcedure,
    ir_cond: &IrCondition,
) -> Condition {
    let [ir_op1, ir_op2] = ir_cond.operands();
    let op1 = make_mir_operand(module, context, proc, ir_op1);
    let op2 = make_mir_operand(module, context, proc, ir_op2);

    let operands = combine_mir_operands(Some(op1), Some(op2));
    use Operands::*;
    match operands {
        Some(
            RegisterRegister(_, _)
            | RegisterMemory(_, _)
            | RegisterImmediate(_, _)
            | MemoryRegister(_, _)
            | MemoryImmediate(_, _),
        ) => (),
        _ => panic!("invalid condition operands"),
    }

    match ir_cond {
        IrCondition::Equals(_, _) => Condition::Equals(operands.unwrap()),
        IrCondition::NotEquals(_, _) => Condition::NotEquals(operands.unwrap()),
    }
}

fn make_mir_operand(
    module: &Module,
    context: &Context,
    proc: &IrProcedure,
    ir_operand: &IrOperand,
) -> Operand {
    match ir_operand {
        &IrOperand::Var(var) => match var {
            Variable::Virtual(virt_id) => {
                let reg_id = proc
                    .data
                    .register_allocation(virt_id)
                    .expect("virtual should be regalloc'd");
                let virt_type = proc
                    .data
                    .virtual_variable_type(virt_id)
                    .expect("virtual should have a type");
                let size = virt_type.sizeof(context).try_into().expect("invalid size");
                Operand::Register(SizedRegister {
                    size,
                    register: Register::variant(reg_id),
                })
            }
            Variable::Data(data_id) => {
                let size = module.data[data_id as usize]
                    .typ()
                    .sizeof(context)
                    .try_into()
                    .expect("invalid size");
                Operand::Memory(SizedMemory {
                    size,
                    memory: Memory::DataRegisterRelative {
                        reg: Register::Rip,
                        data: data_id,
                    },
                })
            }
            Variable::Stack(stack_id) => {
                let size = proc
                    .data
                    .stack_variable_type(stack_id)
                    .expect("missing stack variable type")
                    .sizeof(context)
                    .try_into()
                    .expect("invalid size");
                let rbp_offset = proc
                    .data
                    .stack_data
                    .stack_var_memory_offset(stack_id)
                    .try_into()
                    .unwrap();
                Operand::Memory(SizedMemory {
                    size,
                    memory: Memory::RegisterRelative {
                        reg: Register::Rbp,
                        offset: Offset::I32(rbp_offset),
                    },
                })
            }
            Variable::Register(_) => unreachable!(),
        },
        &IrOperand::Imm(bits) => Operand::Immediate(Immediate {
            bits,
            size: Size::Qword,
        }),
    }
}

fn make_mir_jump_target(proc: &IrProcedure, target: &IrTarget) -> Option<JumpTarget> {
    let target_block_name = target.as_block()?;
    let target_block_id = proc
        .blocks
        .iter()
        .enumerate()
        .find(|(_, b)| b.name == target_block_name)
        .map(|(i, _)| i)?;
    Some(JumpTarget::BlockRelative(target_block_id))
}

fn combine_mir_operands(op1: Option<Operand>, op2: Option<Operand>) -> Option<Operands> {
    use Operand::*;
    let operands = match (op1, op2) {
        (None, None) => Operands::ZeroOperand,
        (Some(Register(reg)), None) => Operands::Register(reg),
        (Some(Register(reg1)), Some(Register(reg2))) => Operands::RegisterRegister(reg1, reg2),
        (Some(Register(reg)), Some(Memory(mem))) => Operands::RegisterMemory(reg, mem),
        (Some(Register(reg)), Some(Immediate(imm))) => Operands::RegisterImmediate(reg, imm),
        (Some(Memory(mem)), None) => Operands::Memory(mem),
        (Some(Memory(mem)), Some(Register(reg))) => Operands::MemoryRegister(mem, reg),
        (Some(Memory(mem)), Some(Immediate(imm))) => Operands::MemoryImmediate(mem, imm),
        (Some(Immediate(imm)), None) => Operands::Immediate(imm),
        _ => return None,
    };

    Some(operands)
}
