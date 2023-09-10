use crate::{
    context::Context,
    data::Value,
    instruction::{Condition, Instruction, Opcode, Operand as IrOperand},
    module::Module,
    procedure::{Block, DataId, Procedure, Variable},
};

use super::{
    mir::{
        instruction::{Immediate, Memory, Operands, SizedMemory, SizedRegister},
        MirBlock, MirInstruction, MirModule, MirProcedure,
    },
    Register, Size,
};

pub fn generate_assembly(module: &Module, mir_module: &MirModule, context: &Context) -> String {
    let mut buf = String::new();

    buf.push_str(
        r#"
.text

#.global _start
#_start:
#    call main
#    jmp _exit


_exit:
    movq %rax, %rdi
    movq $60, %rax
    syscall


.global _handle_fallthrough
_handle_fallthrough:
    movq $-1, %rax
    jmp _exit


"#,
    );

    let procedures = module.procedures.iter().zip(mir_module.procedures.iter());
    for (i, (proc, mir_proc)) in procedures.enumerate() {
        if i != 0 {
            buf.push_str("\n\n");
        }
        ProcAssemblyGen {
            context,
            module,
            proc,
            mir_proc,
        }
        .generate_proc_assembly(&mut buf);
    }

    buf.push_str("\n\n.data\n\n");

    for (i, data) in module.data.iter().enumerate() {
        let data_label = make_data_label(i as DataId);
        buf.push_str(&format!("{}: ", data_label));

        match &data.value {
            Value::U64(v) => {
                buf.push_str(&format!(".quad {}", v));
            }
            Value::Bytes(v) => {
                buf.push_str(".byte ");
                for (i, byte) in v.iter().enumerate() {
                    if i != 0 {
                        buf.push_str(", ");
                    }
                    buf.push_str(&format!("{}", byte));
                }
            }
        };
        buf.push_str("\n");
    }

    return buf;
}

struct ProcAssemblyGen<'a> {
    pub context: &'a Context,
    pub module: &'a Module,
    pub proc: &'a Procedure,
    pub mir_proc: &'a MirProcedure,
}

impl<'a> ProcAssemblyGen<'a> {
    pub fn generate_proc_assembly(&self, buf: &mut String) {
        buf.push_str(&format!(".global {}\n", self.proc.signature.name));
        buf.push_str(&format!("{}:\n", self.proc.signature.name));

        self.generate_prologue(buf);

        let blocks = self.proc.blocks.iter().zip(&self.mir_proc.blocks);
        for (block, mir_block) in blocks {
            self.generate_block_assembly(block, mir_block, buf);
        }

        self.generate_fallthrough_catch(buf);
    }

    pub fn generate_prologue(&self, buf: &mut String) {
        buf.push_str("    pushq %rbp\n");
        buf.push_str("    movq %rsp, %rbp\n");
        buf.push_str(&format!(
            "    subq ${}, %rsp\n",
            self.proc.data.stack_data.total_local_stack_size()
        ));
    }

    pub fn generate_fallthrough_catch(&self, buf: &mut String) {
        buf.push_str("    jmp _handle_fallthrough\n");
    }

    pub fn generate_block_assembly(&self, block: &Block, mir_block: &MirBlock, buf: &mut String) {
        if block.name != "entry" {
            let block_label = make_block_label(self.proc, &block.name);
            buf.push_str(&format!("{}:\n", block_label));
        }

        for (instr, mir) in block.instructions.iter().zip(&mir_block.instrs) {
            self.generate_instruction_assembly(instr, mir, buf);
        }
    }

    pub fn generate_instruction_assembly(
        &self,
        instr: &Instruction,
        mir: &MirInstruction,
        buf: &mut String,
    ) {
        match instr.opcode {
            Opcode::Load | Opcode::Store | Opcode::Move => self.generate_mov(instr, mir, buf),
            Opcode::Convert => self.generate_convert(instr, mir, buf),
            Opcode::Add | Opcode::Sub => self.generate_binary_instruction(instr, mir, buf),
            Opcode::Jump => self.generate_jump(instr, mir, buf),
            Opcode::Call => self.generate_call(instr, buf),
            Opcode::Ret => self.generate_ret(buf),
            Opcode::Address => self.generate_address_instruction(instr, mir, buf),
            op => unimplemented!("{:?}", op),
        }
    }

    pub fn generate_convert(&self, instr: &Instruction, mir: &MirInstruction, buf: &mut String) {
        unimplemented!()
    }

    pub fn generate_address_instruction(
        &self,
        instr: &Instruction,
        mir: &MirInstruction,
        buf: &mut String,
    ) {
        buf.push_str("    leaq ");

        let src = *instr.operands().next().unwrap();
        match src {
            IrOperand::Var(Variable::Data(data_id)) => {
                buf.push_str(&make_data_label(data_id));
            }
            _ => unimplemented!(),
        }
        buf.push_str(", ");

        let dst = instr.dst.unwrap();
        self.generate_operand_assembly(IrOperand::Var(dst), buf);

        buf.push_str("\n");
    }

    pub fn generate_mov(&self, _instr: &Instruction, mir: &MirInstruction, buf: &mut String) {
        buf.push_str("    mov");
        let (dst_size, src_size) = mir.operands.sizes_two().unwrap();
        if dst_size <= src_size {
            match dst_size {
                Size::Qword => buf.push_str("q"),
                Size::Dword => buf.push_str("l"),
                Size::Word => (),
                Size::Byte => buf.push_str("b"),
            }
        } else {
            todo!("support sign/zero extend.");
        }
        self.generate_operands(&mir.operands, buf);
        buf.push_str("\n");
    }

    pub fn generate_call(&self, instr: &Instruction, buf: &mut String) {
        buf.push_str(&format!(
            "    call {}\n",
            instr.target.as_ref().unwrap().as_str()
        ));
    }

    pub fn generate_ret(&self, buf: &mut String) {
        buf.push_str("    leave\n");
        buf.push_str("    ret\n");
    }

    pub fn generate_binary_instruction(
        &self,
        instr: &Instruction,
        mir: &MirInstruction,
        buf: &mut String,
    ) {
        buf.push_str(&format!("    {}q ", instr.opcode.to_str()));

        let mut operands = instr.operands().collect::<Vec<_>>();
        operands.reverse();
        for (i, operand) in operands.into_iter().enumerate() {
            if i != 0 {
                buf.push_str(", ");
            }
            self.generate_operand_assembly(*operand, buf);
        }

        buf.push_str("\n");
    }

    pub fn generate_jump(&self, instr: &Instruction, mir: &MirInstruction, buf: &mut String) {
        if let Some(cond) = instr.cond.as_ref() {
            buf.push_str("    cmpq ");
            let [&a, &b] = cond.operands();
            // Imm values can only be in the first operand

            let (first, second) = if a.is_immediate() { (a, b) } else { (b, a) };
            self.generate_operand_assembly(first, buf);
            buf.push_str(", ");
            self.generate_operand_assembly(second, buf);
            buf.push_str("\n");
        }

        let opcode = match instr.cond.as_ref() {
            None => "jmp",
            Some(Condition::Equals(_, _)) => "je",
            Some(Condition::NotEquals(_, _)) => "jne",
        };
        let block_label = make_block_label(
            self.proc,
            &instr.target.as_ref().unwrap().as_block().unwrap(),
        );
        buf.push_str(&format!("    {opcode} {block_label}\n"));
    }

    pub fn generate_operands(&self, operands: &Operands, buf: &mut String) {
        buf.push_str(" ");

        match operands {
            Operands::ZeroOperand => (),
            Operands::Register(reg) => self.generate_operand_register(reg, buf),
            Operands::RegisterRegister(reg1, reg2) => {
                self.generate_operand_register(reg2, buf);
                buf.push_str(", ");
                self.generate_operand_register(reg1, buf);
            }
            Operands::RegisterMemory(reg, mem) => {
                self.generate_operand_memory(mem, buf);
                buf.push_str(", ");
                self.generate_operand_register(reg, buf);
            }
            Operands::RegisterImmediate(reg, imm) => {
                self.generate_operand_immediate(imm, buf);
                buf.push_str(", ");
                self.generate_operand_register(reg, buf);
            }
            Operands::Memory(mem) => self.generate_operand_memory(mem, buf),
            Operands::MemoryRegister(mem, reg) => {
                self.generate_operand_register(reg, buf);
                buf.push_str(", ");
                self.generate_operand_memory(mem, buf);
            }
            Operands::MemoryImmediate(mem, imm) => {
                self.generate_operand_immediate(imm, buf);
                buf.push_str(", ");
                self.generate_operand_memory(mem, buf);
            }
            Operands::Immediate(imm) => self.generate_operand_immediate(imm, buf),
            // Operands::JumpTarget(target) => (),
            oprnds => unimplemented!("{:?}", oprnds),
        }
    }

    pub fn generate_operand_register(&self, reg: &SizedRegister, buf: &mut String) {
        buf.push_str(reg.register.name(reg.size));
    }

    pub fn generate_operand_memory(&self, mem: &SizedMemory, buf: &mut String) {
        match &mem.memory {
            Memory::RegisterRelative { reg, offset } => {
                buf.push_str(&format!("{}(%{})", offset.as_i32(), reg.name(Size::Qword)));
            }
            Memory::DataRegisterRelative { reg, data } => {
                buf.push_str(&format!(
                    "{}(%{})",
                    make_data_label(*data),
                    reg.name(Size::Qword)
                ));
            }
        }
    }

    pub fn generate_operand_immediate(&self, imm: &Immediate, buf: &mut String) {
        buf.push_str(&format!("${}", imm.bits));
    }

    // TODO remove this and move to mir Operands.
    pub fn generate_operand_assembly(&self, operand: IrOperand, buf: &mut String) {
        match operand {
            IrOperand::Imm(imm) => buf.push_str(&format!("${}", imm)),
            IrOperand::Var(Variable::Virtual(virt_id)) => {
                let register_id = self
                    .proc
                    .data
                    .register_allocation(virt_id)
                    .expect("variable not allocated");
                buf.push_str("%");
                buf.push_str(Register::variant(register_id).name(Size::Qword));
            }
            IrOperand::Var(Variable::Stack(stack_slot_id)) => {
                let offset = self
                    .proc
                    .data
                    .stack_data
                    .stack_var_memory_offset(stack_slot_id);
                buf.push_str(&format!("{}(%rbp)", offset));
            }
            IrOperand::Var(Variable::Data(data_id)) => {
                let data_label = make_data_label(data_id);
                buf.push_str(&format!("({})", data_label));
            }
            oprnd => unimplemented!("{:?}", oprnd),
        }
    }
}

fn make_block_label(proc: &Procedure, block_name: &str) -> String {
    format!("{}_block_{}", proc.signature.name, block_name)
}

fn make_data_label(data_id: DataId) -> String {
    format!("datum_d{}", data_id)
}
