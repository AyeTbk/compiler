use std::{
    collections::HashMap,
    fmt::{Arguments, Error as FmtError, Write},
};

use crate::{
    data::{Data, Value},
    instruction::{Condition, Instruction, Operand, Target},
    module::Module,
    procedure::{
        Block, DataId, ExternalProcedure, Procedure, RegisterId, StackData, StackId, StackVar,
        Variable, VirtualId,
    },
    r#type::Type,
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
        }

        self.writeln()?;

        for proc in &module.procedures {
            self.serialize_proc(proc)?;
            self.writeln()?;
        }

        Ok(())
    }

    fn serialize_data(&mut self, data: &Data, data_id: DataId) -> Result {
        self.write_fmt(format_args!("data d{}", data_id))?;
        self.serialize_type_annotation(&data.typ())?;
        self.write_str(" = ")?;

        match &data.value {
            Value::U64(v) => self.write_fmt(format_args!("{}", v))?,
            Value::Bytes(v) => {
                let s = String::from_utf8(v.clone()).unwrap();
                self.write_fmt(format_args!("{:?}", s))?
            }
        }

        self.writeln_str(";")?;

        Ok(())
    }

    fn serialize_proc(&mut self, proc: &Procedure) -> Result {
        self.write_fmt(format_args!("proc {}", proc.signature.name))?;
        self.serialize_parameter_list(proc, &proc.blocks.entry.parameters)?;
        if let Some(return_typ) = proc.data.return_type.as_ref() {
            self.write_str(" -> ")?;
            self.serialize_typ(return_typ)?;
        }
        self.write_str(" ")?;

        self.block(|this| {
            this.serialize_stack_block(&proc, &proc.data.stack_data)?;
            this.serialize_regalloc_block(&proc.data.register_allocations)?;
            this.serialize_block(proc, &proc.blocks.entry, true)?;
            for block in proc.blocks.others.iter() {
                this.serialize_block(proc, block, false)?;
            }
            Ok(())
        })?;

        Ok(())
    }

    fn serialize_extern_proc(&mut self, proc: &ExternalProcedure) -> Result {
        self.write_fmt(format_args!("extern proc {}", proc.name))?;

        self.write_str("(")?;
        for (i, param_typ) in proc.parameters.iter().enumerate() {
            if i != 0 {
                self.write_str(", ")?;
            }
            self.serialize_typ(param_typ)?;
        }
        self.write_str(")")?;

        if let Some(return_typ) = proc.return_type.as_ref() {
            self.write_str(" -> ")?;
            self.serialize_typ(return_typ)?;
        }
        self.write_str(";")?;
        Ok(())
    }

    fn serialize_typ(&mut self, typ: &Type) -> Result {
        self.write_str(typ.as_str())
    }

    fn serialize_stack_block(&mut self, proc: &Procedure, stack_data: &StackData) -> Result {
        if stack_data.vars.is_empty() {
            return Ok(());
        }

        self.write_str("stack ")?;
        self.block(|this| {
            for (i, stack_var) in stack_data.vars.iter().enumerate() {
                if let Some(typ) = proc.data.stack_variable_type(i as StackId) {
                    this.write_fmt(format_args!("s{i}"))?;
                    this.serialize_type_annotation(&typ)?;
                    this.write_str(" = ")?;
                } else {
                    this.write_fmt(format_args!("s{i} = "))?;
                }
                match &stack_var {
                    StackVar::Local(_) => {
                        this.write_str("local")?;
                    }
                    StackVar::Param(_) => {
                        this.write_str("param")?;
                    }
                    StackVar::CalleeParam {
                        callee_idx,
                        param_idx,
                        ..
                    } => {
                        this.write_fmt(format_args!("callee {}, {}", callee_idx, param_idx))?;
                    }
                }
                this.writeln_str(";")?;
            }
            Ok(())
        })
    }

    fn serialize_regalloc_block(
        &mut self,
        register_allocations: &HashMap<VirtualId, RegisterId>,
    ) -> Result {
        if register_allocations.is_empty() {
            return Ok(());
        }

        self.write_str("regalloc ")?;
        self.block(|this| {
            let mut regallocs = register_allocations.iter().collect::<Vec<_>>();
            regallocs.sort_by_key(|&(v, _)| *v);

            for (virt, reg) in regallocs {
                this.writeln_str(&format!("v{} = r{};", virt, reg))?;
            }

            Ok(())
        })
    }

    fn serialize_block(&mut self, proc: &Procedure, block: &Block, is_entry: bool) -> Result {
        self.write_fmt(format_args!("{}", block.name))?;
        if !is_entry {
            self.serialize_parameter_list(proc, &block.parameters)?;
        }
        self.write_str(" ")?;
        self.block(|this| {
            for instruction in block.instructions.iter() {
                this.serialize_instruction(proc, instruction)?;
                this.writeln()?;
            }
            Ok(())
        })
    }

    fn serialize_parameter_list(&mut self, proc: &Procedure, parameters: &[Variable]) -> Result {
        self.write_str("(")?;
        for (i, parameter) in parameters.iter().enumerate() {
            if i != 0 {
                self.write_str(", ")?;
            }
            self.serialize_variable(&parameter)?;
            let typ = proc
                .data
                .virtual_variable_type(parameter.as_virtual().unwrap());
            self.serialize_type_annotation(typ.as_ref().unwrap())?;
        }
        self.write_str(")")
    }

    fn serialize_instruction(&mut self, proc: &Procedure, instruction: &Instruction) -> Result {
        if let Some(dst) = &instruction.dst {
            self.serialize_variable(dst)?;

            if let Some(virt_id) = dst.as_virtual() {
                if let Some(typ) = proc.data.virtual_variable_type(virt_id) {
                    self.serialize_type_annotation(&typ)?;
                }
            }

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

    fn serialize_type_annotation(&mut self, typ: &Type) -> Result {
        self.write_str(": ")?;
        self.serialize_typ(typ)
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
