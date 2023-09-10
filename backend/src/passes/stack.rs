// use std::collections::HashMap;

// use crate::{
//     instruction::{Instruction, Opcode, Operand},
//     procedure::{Procedure, Variable},
//     utils::Peephole,
// };

// pub fn allstack_setup_callees(declarations: &Declarations, proc: &mut Procedure) {
//     let mut new_stack_vars: HashMap<Variable, Variable> = HashMap::new();

//     Peephole::peep_instructions(proc, |ph, i, instr| {
//         if instr.opcode == Opcode::Call {
//             let name = instr
//                 .target
//                 .as_ref()
//                 .expect("call instr should have a target")
//                 .as_procedure()
//                 .expect("call instr should have a procedure target");

//             let target_proc_decl = declarations
//                 .get_procedure(name)
//                 .expect("target proc should be declared");

//             assert_eq!(target_proc_decl.parameters.len(), instr.operands().count());
//             assert_eq!(target_proc_decl.returns.len(), instr.dst.iter().count());

//             assert!(
//                 matches!(
//                     target_proc_decl.calling_convention,
//                     Some(CallingConventionId::AllStack)
//                 ),
//                 "target proc call conv should be allstack"
//             );

//             // Allocate a StackCall and Call stack vars for the operands and returns.
//             // Insert stores for the args and update them to the new stack var.
//             // Update the returns to be their allocated ones and update later uses of the originals.
//             let call_stack_size =
//                 ((instr.operands().count() + instr.dst.iter().count()) * 8) as u32; // FIXME this sucks
//             let call_idx = ph.proc_data.stack_data.allocate_call(call_stack_size);
//             let mut ordinal = 0;
//             for arg_operand in instr.operands_mut() {
//                 let new_stack_id = ph
//                     .proc_data
//                     .stack_data
//                     .allocate_call_stack_var(call_idx, ordinal);
//                 ordinal += 1;
//                 let new_stack_var = Variable::Stack(new_stack_id);

//                 let actual_arg_operand = if let Some(arg_var) = arg_operand.as_variable() {
//                     new_stack_vars
//                         .get(&arg_var)
//                         .map(|&v| Operand::Var(v))
//                         .unwrap_or(*arg_operand)
//                 } else {
//                     *arg_operand
//                 };

//                 ph.insert_before(i, Instruction::store(new_stack_id, actual_arg_operand));
//                 *arg_operand = Operand::Var(new_stack_var);
//             }
//             for ret in &mut instr.dst {
//                 let new_stack_var = ph
//                     .proc_data
//                     .stack_data
//                     .allocate_call_stack_var(call_idx, ordinal);
//                 ordinal += 1;
//                 let new_stack_call_var = Variable::Stack(new_stack_var);

//                 // Return values are moved to a dedicated local stack slot because otherwise,
//                 // multiple calls could overwrite them.
//                 let new_stack_slot = ph.proc_data.stack_data.allocate_local_stack_slot();
//                 let new_stack_slot_var = Variable::Stack(new_stack_slot);

//                 ph.insert_after(i, Instruction::store(new_stack_slot, new_stack_call_var));

//                 new_stack_vars.insert(*ret, new_stack_slot_var);
//                 *ret = new_stack_call_var;
//             }
//         }

//         // Update operands
//         for operand in instr.operands_mut() {
//             match operand {
//                 Operand::Var(operand_var) => {
//                     let Some(new_var) = new_stack_vars.get(operand_var) else { continue };
//                     *operand_var = *new_var;
//                 }
//                 _ => (),
//             }
//         }
//         // Update returns
//         for ret in &mut instr.dst {
//             let Some(new_var) = new_stack_vars.get(ret) else { continue };
//             *ret = *new_var;
//         }
//         // Update cond
//         for cond_operand in instr.cond.iter_mut().flat_map(|c| c.operands_mut()) {
//             match cond_operand {
//                 Operand::Var(cond_var) => {
//                     let Some(new_var) = new_stack_vars.get(cond_var) else { continue };
//                     *cond_var = *new_var;
//                 }
//                 _ => (),
//             }
//         }
//     });
// }

// pub fn allstack_allocate_parameters_and_return(proc: &mut Procedure) {
//     // NOTE: Loads and Stores will be handled by another pass.

//     // Allocate a stack slot for every proc parameter.
//     // Only support virtual variables, panic on non virtual variables.
//     // Replace every proc parameter with the allocated stack variable.
//     let mut new_stack_vars = HashMap::new();
//     for param in proc.blocks.entry_parameters_mut() {
//         if !param.variable.is_virtual() {
//             unimplemented!("non virtual variables not supported");
//         }

//         let param_stack_slot = proc.data.stack_data.allocate_caller_stack_slot();
//         let param_stack_var = Variable::Stack(param_stack_slot);
//         new_stack_vars.insert(param.variable, param_stack_var);
//         param.variable = param_stack_var;
//     }

//     for _ret in &mut proc.signature.returns {
//         // let ret_stack_slot = proc.data.stack_data.allocate_caller_stack_slot();
//         // let ret_stack_var = Variable::Stack(ret_stack_slot);
//         // ret.variable = ret_stack_var;
//         unimplemented!();
//     }

//     // Modify every use of the proc parameters so they use the stack variable.
//     Peephole::peep_instructions(proc, |_, _, instr| {
//         for operand in instr.operands_mut() {
//             if let Some(operand_var) = operand.as_variable() {
//                 if let Some(stack_var) = new_stack_vars.get(&operand_var) {
//                     *operand = Operand::Var(*stack_var);
//                 }
//             }
//         }

//         for operand in instr
//             .cond
//             .as_mut()
//             .iter_mut()
//             .flat_map(|c| c.operands_mut().into_iter())
//         {
//             if let Some(operand_var) = operand.as_variable() {
//                 if let Some(stack_var) = new_stack_vars.get(&operand_var) {
//                     *operand = Operand::Var(*stack_var);
//                 }
//             }
//         }
//     });
// }

// pub fn spill_all_virtual(proc: &mut Procedure) {
//     // NOTE: Loads and Stores will be handled by another pass.

//     // FIXME entry no longer has parameters, update this
//     // FIXME dont touch call instr operands, they are determined by the callee call convention.

//     let mut new_stack_vars: HashMap<Variable, Variable> = HashMap::new();

//     // 1. Handle block parameters
//     Peephole::peep_blocks(proc, |ph, _, block| {
//         // allocate stack slot for all virtual parameters.
//         // replace parameter with new stack variable.
//         for param in &mut block.parameters {
//             if param.variable.is_virtual() {
//                 let stack_slot = ph.proc_data.stack_data.allocate_local_stack_slot();
//                 let stack_var = Variable::Stack(stack_slot);
//                 new_stack_vars.insert(param.variable, stack_var);
//                 param.variable = stack_var;
//             }
//         }
//     });

//     // 2. Handle instructions
//     Peephole::peep_instructions(proc, |ph, _, instr| {
//         // If dest is some and not stack:
//         //  allocate a stack slot for it.
//         //  replace it with the new stack variable.
//         if let Some(dest_var) = instr.dst.as_mut() {
//             if dest_var.is_virtual() {
//                 let stack_slot = ph.proc_data.stack_data.allocate_local_stack_slot();
//                 let stack_var = Variable::Stack(stack_slot);
//                 new_stack_vars.insert(*dest_var, stack_var);
//                 *dest_var = stack_var;
//             }
//         }

//         // For every operand:
//         //  replace with corresponding previously allocated stack variable.
//         for operand in instr.operands_mut() {
//             if let Some(operand_var) = operand.as_variable() {
//                 if !operand_var.is_virtual() {
//                     continue;
//                 }
//                 if let Some(stack_var) = new_stack_vars.get(&operand_var) {
//                     *operand = Operand::Var(*stack_var);
//                 }
//             }
//         }

//         // Handle condition operands
//         if let Some(cond) = &mut instr.cond {
//             for operand in cond.operands_mut() {
//                 if let Some(operand_var) = operand.as_variable() {
//                     if operand_var.is_virtual() {
//                         if let Some(stack_var) = new_stack_vars.get(&operand_var) {
//                             *operand = Operand::Var(*stack_var);
//                         }
//                     }
//                 }
//             }
//         }
//     });
// }

// pub fn generate_loads_stores(proc: &mut Procedure) {
//     // For every instruction:
//     //   if instr is a jump to another block:
//     //     for every operand:
//     //       - panic if not a stack operand (handle that case later).
//     //       - load from the stack operand to a new virtual variable.
//     //       - make sure the associated target block parameter is a stack var (else panic! handle that later).
//     //       - store the new virtual variable to the associated target block parameter.
//     //       - replace the operand with the asssociated target block parameter.
//     //   else:
//     //     - insert loads to new virtual variables before every use of a given stack operand,
//     //        and replace said operand with new virtual variable.
//     //     - replace present dst with new virtual variable and insert stores for them after the
//     //        instruction. Remember what virtual var maps to what stack var for later substitutions.
//     //   - handle condition operands like non jump instr operand above.
//     let mut block_parameters = HashMap::new();
//     for block in proc.blocks.iter() {
//         block_parameters.insert(block.name.clone(), block.parameters.clone());
//     }

//     Peephole::peep_instructions(proc, |ph, i, instr| {
//         // Handle source operands
//         if instr.opcode == Opcode::Jump {
//             let target_block_name = instr.target.as_ref().and_then(|t| t.as_block()).unwrap();
//             let target_paramters = block_parameters.get(target_block_name).unwrap();
//             assert_eq!(instr.operands().count(), target_paramters.len());
//             for (operand, param) in instr.operands_mut().zip(target_paramters) {
//                 let operand_stack_slot = operand
//                     .as_variable()
//                     .and_then(|v| v.as_stack())
//                     .expect("non stack operand not currently supported");
//                 let param_stack_slot = param
//                     .variable
//                     .as_stack()
//                     .expect("non stack block parameter not currently supported");
//                 let new_virtual_var = ph.proc_data.acquire_new_virtual_variable();
//                 ph.insert_before(i, Instruction::load(new_virtual_var, operand_stack_slot));
//                 ph.insert_before(i, Instruction::store(param_stack_slot, new_virtual_var));
//                 *operand = Operand::Var(param.variable);
//             }

//             // Handle condition operands
//             if let Some(cond) = &mut instr.cond {
//                 for operand in cond.operands_mut() {
//                     if let Some(operand_var) = operand.as_variable() {
//                         if let Some(stack_slot) = operand_var.as_stack() {
//                             let new_virtual_var = ph.proc_data.acquire_new_virtual_variable();
//                             *operand = Operand::Var(new_virtual_var);
//                             ph.insert_before(i, Instruction::load(new_virtual_var, stack_slot));
//                         }
//                     }
//                 }
//             }
//         } else if !matches!(
//             instr.opcode,
//             Opcode::Call | Opcode::Ret | Opcode::Load | Opcode::Store
//         ) {
//             for operand in instr.operands_mut() {
//                 if let Some(operand_var) = operand.as_variable() {
//                     if let Some(stack_slot) = operand_var.as_stack() {
//                         let new_virtual_var = ph.proc_data.acquire_new_virtual_variable();
//                         *operand = Operand::Var(new_virtual_var);
//                         ph.insert_before(i, Instruction::load(new_virtual_var, stack_slot));
//                     }
//                 }
//             }

//             // Handle destination
//             if !matches!(
//                 instr.opcode,
//                 Opcode::Call | Opcode::Ret | Opcode::Load | Opcode::Store
//             ) {
//                 if let Some(dst_var) = &mut instr.dst {
//                     if let Some(stack_var_id) = dst_var.as_stack() {
//                         let new_virtual_var = ph.proc_data.acquire_new_virtual_variable();
//                         *dst_var = new_virtual_var;
//                         ph.insert_after(i, Instruction::store(stack_var_id, new_virtual_var));
//                     }
//                 }
//             }
//         }
//     });

//     store_stack_rets(proc);
// }

// fn store_stack_rets(proc: &mut Procedure) {
//     // NOTE Currently assumes a single return value

//     // Insert appropriate store instructions for stack allocated return values

//     Peephole::peep_instructions(proc, |ph, _i, instr| {
//         if instr.opcode != Opcode::Ret {
//             return;
//         }

//         let returns = ph
//             .proc_signature
//             .returns
//             .iter()
//             .cloned()
//             .collect::<Vec<_>>();
//         let operand_count = instr.operands().count();

//         assert_eq!(operand_count, returns.len());

//         for (_operand, _ret_param) in instr.operands_mut().zip(returns) {
//             // if let Some(stack_slot_id) = ret_param.variable.as_stack() {
//             //     ph.insert_before(i, Instruction::store(stack_slot_id, *operand));
//             //     *operand = Operand::Var(ret_param.variable);
//             // }
//             unimplemented!();
//         }
//     });
// }

// pub fn fix_memory_to_memory_loads_stores(proc: &mut Procedure) {
//     Peephole::peep_instructions(proc, |ph, i, instr| {
//         if !matches!(instr.opcode, Opcode::Load | Opcode::Store) {
//             return;
//         }
//         // Transforms
//         //     s1 = load s0;   or   s1 = store s0;
//         // into the form
//         //     v0 = load s0;
//         //     s1 = store v0;

//         let src = instr
//             .src
//             .iter_mut()
//             .next()
//             .expect("load/store instr should have a src operand");
//         let dst = instr
//             .dst
//             .as_mut()
//             .expect("load/store instr should have a src operand");

//         match (&src, &dst) {
//             (Operand::Var(Variable::Stack(src_id)), Variable::Stack(_)) => {
//                 let new_virt_var = ph.proc_data.acquire_new_virtual_variable();

//                 ph.insert_before(i, Instruction::load(new_virt_var, *src_id));

//                 instr.opcode = Opcode::Store;
//                 *src = Operand::Var(new_virt_var);
//             }
//             _ => (),
//         }
//     });
// }
