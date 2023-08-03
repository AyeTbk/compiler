use std::fmt::{Arguments, Error as FmtError, Write};

use crate::{
    instruction::{Condition, Instruction, Operand, Target},
    module::{Data, Module},
    procedure::{
        Block, DataId, ExternalProcedure, Parameter, Procedure, StackData, StackSlotKind, StackVar,
        Variable,
    },
};

type Result = std::result::Result<(), FmtError>;

pub struct ModuleSerializer<'a, W: Write> {
    w: &'a mut W,
    line_just_started: bool,
    indent_str: &'a str,
    indent_depth: i32,
}

impl<'a, W: Write> ModuleSerializer<'a, W> {
    pub fn new(w: &'a mut W) -> Self {
        Self {
            w,
            line_just_started: true,
            indent_str: "    ",
            indent_depth: 0,
        }
    }

    pub fn serialize(&mut self, module: &Module) {
        self.serialize_module(module).unwrap();
    }

    fn serialize_module(&mut self, module: &Module) -> Result {
        for exproc in &module.external_procedures {
            self.serialize_extern_proc(exproc)?;
            self.writeln()?;
        }

        self.writeln()?;

        for (i, data) in module.data.iter().enumerate() {
            self.serialize_data(data, i as DataId)?;
            self.writeln()?;
        }

        self.writeln()?;

        for proc in &module.procedures {
            self.serialize_proc(proc)?;
            self.writeln()?;
        }

        Ok(())
    }

    fn serialize_data(&mut self, data: &Data, data_id: DataId) -> Result {
        self.write_fmt(format_args!("data d{}: u64 = {};", data_id, data.value))?;

        Ok(())
    }

    fn serialize_proc(&mut self, proc: &Procedure) -> Result {
        self.write_fmt(format_args!("proc {}", proc.signature.name))?;
        self.serialize_parameter_list(&proc.blocks.entry.parameters)?;
        if !proc.signature.returns.is_empty() {
            self.write_str(" -> u64")?;
        }
        self.write_str(" ")?;

        self.block(|this| {
            this.serialize_stack_block(&proc.data.stack_data)?;
            this.serialize_block(&proc.blocks.entry, true)?;
            for block in proc.blocks.others.iter() {
                this.serialize_block(block, false)?;
            }
            Ok(())
        })?;

        Ok(())
    }

    fn serialize_extern_proc(&mut self, proc: &ExternalProcedure) -> Result {
        self.write_fmt(format_args!("extern proc {}", proc.name))?;

        self.write_str("(")?;
        for (i, _param_typ) in proc.parameters.iter().enumerate() {
            if i != 0 {
                self.write_str(", ")?;
            }
            self.write_str("u64")?;
        }
        self.write_str(")")?;

        if !proc.returns.is_empty() {
            self.write_str(" -> u64")?;
        }
        self.write_str(";")?;
        Ok(())
    }

    fn serialize_stack_block(&mut self, stack_data: &StackData) -> Result {
        if stack_data.slots.is_empty() {
            return Ok(());
        }

        self.write_str("stack ")?;
        self.block(|this| {
            for (i, stack_call) in stack_data.calls.iter().enumerate() {
                this.writeln_str(&format!("call {}, {};", i, stack_call.size))?;
            }
            for (i, stack_var) in stack_data.vars.iter().enumerate() {
                this.write_fmt(format_args!("s{i} = "))?;
                match &stack_var {
                    StackVar::Call { call_idx, ordinal } => {
                        this.write_fmt(format_args!("call {}, {}", call_idx, ordinal))?;
                    }
                    StackVar::Slot(slot_idx) => {
                        let slot = &stack_data.slots[*slot_idx];
                        match &slot.kind {
                            StackSlotKind::Local => {
                                this.write_str("local")?;
                            }
                            StackSlotKind::Caller => {
                                this.write_str("caller")?;
                            }
                        }
                    }
                }
                this.writeln_str(";")?;
            }
            Ok(())
        })
    }

    fn serialize_block(&mut self, block: &Block, is_entry: bool) -> Result {
        self.write_fmt(format_args!("{}", block.name))?;
        if !is_entry {
            self.serialize_parameter_list(&block.parameters)?;
        }
        self.write_str(" ")?;
        self.block(|this| {
            for instruction in block.instructions.iter() {
                this.serialize_instruction(instruction)?;
                this.writeln()?;
            }
            Ok(())
        })
    }

    fn serialize_parameter_list(&mut self, parameters: &[Parameter]) -> Result {
        self.write_str("(")?;
        for (i, parameter) in parameters.iter().enumerate() {
            if i != 0 {
                self.write_str(", ")?;
            }
            self.serialize_variable(&parameter.variable)?;
            self.serialize_type_annotation()?;
        }
        self.write_str(")")
    }

    fn serialize_instruction(&mut self, instruction: &Instruction) -> Result {
        if let Some(dst) = &instruction.dst {
            self.serialize_variable(dst)?;
            self.write_str(" = ")?;
        }
        self.write_str(instruction.opcode.to_str())?;

        if let Some(target) = &instruction.target {
            match target {
                Target::Block(_) => self.write_str(" #")?,
                Target::Procedure(_) => self.write_str(" @")?,
            }
            self.write_str(&target.as_str())?;
            if !instruction.src.operands.is_empty() {
                self.write_str("(")?;
            }
        }

        if instruction.operands().count() > 0 && instruction.target.is_none() {
            self.write_str(" ")?;
        }
        for (i, operand) in instruction.operands().enumerate() {
            if i != 0 {
                self.write_str(", ")?;
            }
            self.serialize_operand(operand)?;
        }

        if instruction.target.is_some() && !instruction.src.operands.is_empty() {
            self.write_str(")")?;
        }

        if let Some(cond) = &instruction.cond {
            self.write_str(" ")?;
            self.serialize_condition(cond)?;
        }

        self.write_str(";")
    }

    fn serialize_condition(&mut self, cond: &Condition) -> Result {
        self.write_str("if ")?;
        match cond {
            Condition::Equals(a, b) => {
                self.serialize_operand(a)?;
                self.write_str(" == ")?;
                self.serialize_operand(b)?;
            }
            Condition::NotEquals(a, b) => {
                self.serialize_operand(a)?;
                self.write_str(" != ")?;
                self.serialize_operand(b)?;
            }
        }
        Ok(())
    }

    fn serialize_operand(&mut self, operand: &Operand) -> Result {
        match operand {
            Operand::Var(var) => self.serialize_variable(var),
            Operand::Imm(imm) => self.write_fmt(format_args!("{}", imm)),
        }
    }

    fn serialize_variable(&mut self, variable: &Variable) -> Result {
        match variable {
            Variable::Virtual(id) => self.write_fmt(format_args!("v{id}")),
            Variable::Register(id) => self.write_fmt(format_args!("r{id}")),
            Variable::Stack(id) => self.write_fmt(format_args!("s{id}")),
            Variable::Data(id) => self.write_fmt(format_args!("d{id}")),
        }
    }

    fn serialize_type_annotation(&mut self) -> Result {
        self.write_str(": u64")
    }

    fn indent(&mut self) {
        self.indent_depth += 1;
    }

    fn dedent(&mut self) {
        self.indent_depth -= 1;
    }

    fn write_fmt(&mut self, a: Arguments) -> Result {
        self.indent_check()?;
        self.w.write_fmt(a)
    }

    fn _writeln_fmt(&mut self, a: Arguments) -> Result {
        self.write_fmt(a)?;
        self.writeln()
    }

    fn write_str(&mut self, s: &str) -> Result {
        self.indent_check()?;
        self.w.write_str(s)
    }

    fn writeln_str(&mut self, s: &str) -> Result {
        self.write_str(s)?;
        self.writeln()
    }

    fn write_indentation(&mut self) -> Result {
        for _ in 0..self.indent_depth {
            self.w.write_str(self.indent_str)?;
        }
        Ok(())
    }

    fn writeln(&mut self) -> Result {
        self.line_just_started = true;
        self.w.write_str("\n")
    }

    fn block(&mut self, f: impl FnOnce(&mut Self) -> Result) -> Result {
        self.writeln_str("{")?;
        self.indent();
        f(self)?;
        self.dedent();
        self.writeln_str("}")?;
        Ok(())
    }

    fn indent_check(&mut self) -> Result {
        if self.line_just_started {
            self.write_indentation()?;
            self.line_just_started = false;
        }
        Ok(())
    }
}
