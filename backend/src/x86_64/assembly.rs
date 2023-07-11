use crate::{
    instruction::{Condition, Instruction, Opcode, Operand},
    module::Module,
    procedure::{Block, Procedure, Variable},
};

use super::Isa;

pub fn generate_assembly(module: &Module) -> String {
    let mut buf = String::new();

    buf.push_str(
        r#"
.global _start
_start:
    call main
    popq %rax
    jmp _exit


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

    for (i, proc) in module.procedures.iter().enumerate() {
        if i != 0 {
            buf.push_str("\n\n");
        }
        generate_proc_assembly(&proc, &mut buf);
    }

    return buf;
}

fn generate_proc_assembly(proc: &Procedure, buf: &mut String) {
    buf.push_str(&format!(".global {}\n", proc.signature.name));
    buf.push_str(&format!("{}:\n", proc.signature.name));

    generate_proc_prologue(proc, buf);

    for block in proc.blocks.iter() {
        generate_block_assembly(proc, block, buf);
    }

    generate_fallthrough_catch(proc, buf);
}

fn generate_proc_prologue(proc: &Procedure, buf: &mut String) {
    buf.push_str("    pushq %rbp\n");
    buf.push_str("    movq %rsp, %rbp\n");
    buf.push_str(&format!(
        "    subq ${}, %rsp\n",
        proc.data.stack_data.total_local_stack_size()
    ));
}

fn generate_fallthrough_catch(_proc: &Procedure, buf: &mut String) {
    buf.push_str("    jmp _handle_fallthrough\n");
}

fn generate_block_assembly(proc: &Procedure, block: &Block, buf: &mut String) {
    if block.name != "entry" {
        let block_label = make_block_label(proc, &block.name);
        buf.push_str(&format!("{}:\n", block_label));
    }

    for instr in &block.instructions {
        generate_instruction_assembly(proc, block, instr, buf);
    }
}

fn generate_instruction_assembly(
    proc: &Procedure,
    block: &Block,
    instr: &Instruction,
    buf: &mut String,
) {
    match instr.opcode {
        Opcode::Load | Opcode::Store => generate_mov(proc, instr, buf),
        Opcode::Add | Opcode::Sub => generate_binary_instruction(proc, instr, buf),
        Opcode::Jump => generate_jump(proc, block, instr, buf),
        Opcode::Call => generate_call(instr, buf),
        Opcode::Ret => generate_ret(buf),
        op => unimplemented!("{:?}", op),
    }
}

fn generate_mov(proc: &Procedure, instr: &Instruction, buf: &mut String) {
    buf.push_str("    movq ");

    let src = *instr.operands().next().unwrap();
    generate_operand_assembly(proc, src, buf);
    buf.push_str(", ");

    let dst = instr.dst.unwrap();
    generate_operand_assembly(proc, Operand::Var(dst), buf);

    buf.push_str("\n");
}

fn generate_call(instr: &Instruction, buf: &mut String) {
    buf.push_str(&format!(
        "    call {}\n",
        instr.target.as_ref().unwrap().as_str()
    ));
}

fn generate_ret(buf: &mut String) {
    buf.push_str("    leave\n");
    buf.push_str("    ret\n");
}

fn _generate_nullary_instruction(instr: &Instruction, buf: &mut String) {
    buf.push_str("    ");
    buf.push_str(instr.opcode.to_str());
    buf.push_str("\n");
}

fn generate_binary_instruction(proc: &Procedure, instr: &Instruction, buf: &mut String) {
    buf.push_str(&format!("    {}q ", instr.opcode.to_str()));

    let mut operands = instr.operands().collect::<Vec<_>>();
    operands.reverse();
    for (i, operand) in operands.into_iter().enumerate() {
        if i != 0 {
            buf.push_str(", ");
        }
        generate_operand_assembly(proc, *operand, buf);
    }

    buf.push_str("\n");
}

fn generate_jump(proc: &Procedure, _block: &Block, instr: &Instruction, buf: &mut String) {
    if let Some(cond) = instr.cond.as_ref() {
        buf.push_str("    cmpq ");
        let [&a, &b] = cond.operands();
        // Imm values can only be in the first operand

        let (first, second) = if a.is_immediate() { (a, b) } else { (b, a) };
        generate_operand_assembly(proc, first, buf);
        buf.push_str(", ");
        generate_operand_assembly(proc, second, buf);
        buf.push_str("\n");
    }

    let opcode = match instr.cond.as_ref() {
        None => "jmp",
        Some(Condition::Equals(_, _)) => "je",
        Some(Condition::NotEquals(_, _)) => "jne",
    };
    let block_label = make_block_label(proc, &instr.target.as_ref().unwrap().as_block().unwrap());
    buf.push_str(&format!("    {opcode} {block_label}\n"));
}

fn generate_operand_assembly(proc: &Procedure, operand: Operand, buf: &mut String) {
    match operand {
        Operand::Imm(imm) => buf.push_str(&format!("${}", imm)),
        Operand::Var(Variable::Register(register_id)) => {
            buf.push_str("%");
            buf.push_str(Isa::register_name(register_id));
        }
        Operand::Var(Variable::Stack(stack_slot_id)) => {
            let offset = proc.data.stack_data.stack_var_memory_offset(stack_slot_id);
            buf.push_str(&format!("{}(%rbp)", offset));
        }
        oprnd => unimplemented!("{:?}", oprnd),
    }
}

fn make_block_label(proc: &Procedure, block_name: &str) -> String {
    format!("{}_block_{}", proc.signature.name, block_name)
}
