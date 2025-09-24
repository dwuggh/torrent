use std::collections::HashMap;

use cranelift::codegen::ir::{BlockCall, JumpTableData, TrapCode};
use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;
use rustc_hash::FxHashSet;

use crate::core::compiler::error::{CodegenError, CodegenResult};
use crate::core::function::FunctionSignature;
use crate::core::function::LispFunction;
use crate::core::ident::Ident;
use crate::core::object::NIL;
use crate::core::object::TRUE;
use crate::core::parser::expr::*;
use crate::core::symbol::LispSymbol;
use crate::core::symbol::Symbol;
use crate::core::Tagged;

#[derive(Debug)]
pub struct UnresolvedClosure {
    func: LispFunction,
    unresolved: FxHashSet<Arg>,
}

#[allow(unused)]
#[derive(Debug)]
pub enum Closure {
    Unresolved(UnresolvedClosure),
    Resolved(LispFunction),
}

enum IncomingArgs<'a> {
    Trampoline { args_ptr: Value, args_cnt: Value },
    Direct { args: &'a [Value] },
}

fn get_locals(locals: &Vec<(Arg, Variable)>, target: &Arg) -> Option<Variable> {
    let mut i = locals.len();
    while i > 0 {
        i = i - 1;
        let (arg, var) = &locals[i];
        if arg == target {
            return Some(*var);
        }
    }
    None
}

impl UnresolvedClosure {
    fn resolve(self, codegen: &mut Codegen) -> Closure {
        let mut unresolved = FxHashSet::default();
        // unresolved.into_iter().partition()

        for arg in self.unresolved {
            // Try to resolve the argument to a Variable in the current codegen context
            if let Some(variable) = get_locals(&codegen.locals, &arg) {
                // Get the current value of the variable
                let value = codegen.builder.use_var(variable);
                let symbol: Symbol = arg.ident.into();
                let sym = codegen.translate_lispobj(&LispSymbol::from(symbol));
                let func_val = codegen.translate_lispobj(&self.func);
                codegen.call_internal("store_captured", &[sym, value, func_val]);
            } else {
                // Still can't resolve this argument, keep it unresolved
                unresolved.insert(arg);
            }
        }

        if unresolved.is_empty() {
            Closure::Resolved(self.func)
        } else {
            Closure::Unresolved(Self {
                func: self.func,
                unresolved,
            })
        }
    }
}

pub struct Codegen<'a> {
    module: &'a mut JITModule,
    data_desc: &'a mut DataDescription,
    builder: FunctionBuilder<'a>,
    func: LispFunction,
    locals: Vec<(Arg, Variable)>,
    captures: FxHashSet<Arg>,
    pub func_id: FuncId,
    env: Value,
    builtin_funcs: &'a HashMap<String, FuncId>,
    defined_funcs: Vec<Closure>,
}

impl<'a> Codegen<'a> {
    pub fn new_empty<'s>(
        module: &'a mut JITModule,
        data_desc: &'a mut DataDescription,
        builtin_funcs: &'a HashMap<String, FuncId>,
        fctx: &'a mut FunctionBuilderContext,
        ctx: &'a mut Context,
    ) -> CodegenResult<Self> {
        // make signature
        let sig = &mut ctx.func.signature;
        // env arg
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        tracing::debug!(
            "Function signature created with {} params, {} returns",
            sig.params.len(),
            sig.returns.len()
        );

        let func_id = module.declare_anonymous_function(&ctx.func.signature)?;
        tracing::debug!("Declared anonymous function with id: {:?}", func_id);

        let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // builder.block_params(entry_block);

        let block_params = builder.block_params(entry_block);
        tracing::debug!("Entry block has {} parameters", block_params.len());

        let env = block_params[0];

        let func = LispFunction::new_closure(func_id, FunctionSignature::default());
        // let closure_val = translate_value(&mut builder, func_runtime_val);

        let codegen = Self {
            module,
            data_desc,
            builtin_funcs,
            builder,
            func,

            locals: Vec::new(),
            captures: Default::default(),
            env,
            func_id,
            defined_funcs: Vec::new(),
        };

        Ok(codegen)
    }

    pub fn new<'s>(
        module: &'a mut JITModule,
        data_desc: &'a mut DataDescription,
        builtin_funcs: &'a HashMap<String, FuncId>,
        fctx: &'a mut FunctionBuilderContext,
        ctx: &'a mut Context,
        lambda: &Lambda,
    ) -> CodegenResult<Self> {
        tracing::debug!("Creating new codegen with config: {:?}", lambda);

        let argc = lambda.args.total();
        let use_trampoline = lambda.args.use_trampoline();

        // make signature
        let sig = &mut ctx.func.signature;

        // TODO move this as function for FunctionSignature

        if lambda.args.use_trampoline() {
            // args_ptr: pointer to arguments array
            sig.params.push(AbiParam::new(types::I64));
            // args_cnt: number of arguments
            sig.params.push(AbiParam::new(types::I64));
            // env arg
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        } else {
            for _ in 0..argc {
                sig.params.push(AbiParam::new(types::I64));
            }

            // env arg
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }

        tracing::debug!(
            "Function signature created with {} params, {} returns(use trampoline: {use_trampoline})",
            sig.params.len(),
            sig.returns.len()
        );

        let func_id = module.declare_anonymous_function(&ctx.func.signature)?;
        tracing::debug!("Declared anonymous function with id: {:?}", func_id);

        let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // builder.block_params(entry_block);

        let params = builder.block_params(entry_block).to_vec();

        let env = if use_trampoline {
            params
                .get(2)
                .copied()
                .unwrap_or_else(|| panic!("expected env parameter for trampoline"))
        } else {
            params
                .last()
                .copied()
                .unwrap_or_else(|| panic!("expected env parameter for direct call"))
        };

        let locals = Vec::new();
        let signature = FunctionSignature::from_args(&lambda.args);
        let func = LispFunction::new_closure(func_id, signature);

        let mut codegen = Self {
            module,
            data_desc,
            builtin_funcs,
            builder,
            locals,
            func,
            env,
            func_id,
            defined_funcs: Vec::new(),
            captures: Default::default(),
        };

        if use_trampoline {
            let args_ptr = params[0];
            let args_cnt = params[1];
            codegen.setup_locals(lambda, IncomingArgs::Trampoline { args_ptr, args_cnt })?;
        } else {
            let env_index = params.len().saturating_sub(1);
            let direct_args = &params[..env_index];
            codegen.setup_locals(lambda, IncomingArgs::Direct { args: direct_args })?;
        }

        Ok(codegen)
    }

    fn nil(&mut self) -> Value {
        self.builder.ins().iconst(types::I64, NIL)
    }

    fn t(&mut self) -> Value {
        // let t = LispValue
        self.builder.ins().iconst(types::I64, TRUE)
    }

    fn not_nil(&mut self, val: Value) -> Value {
        let nil = self.nil();
        self.builder.ins().icmp(IntCC::NotEqual, val, nil)
    }

    pub fn translate_progn<'s>(&mut self, progn: &Progn) -> CodegenResult<Value> {
        self.translate_exprs(&progn.body)
    }

    pub fn translate_exprs<'s>(&mut self, exprs: &[Expr]) -> CodegenResult<Value> {
        // TODO need to drop other values
        exprs
            .iter()
            .map(|e| self.translate_expr(e))
            .last()
            .unwrap_or(Ok(self.nil()))
    }

    pub fn finalize(mut self, val: Value) -> (UnresolvedClosure, Vec<Closure>) {
        self.builder.ins().return_(&[val]);
        self.builder.finalize();

        let collections = self.defined_funcs;
        let this = UnresolvedClosure {
            func: self.func,
            unresolved: self.captures,
        };

        (this, collections)
    }

    fn call_internal(&mut self, func_name: &str, args: &[Value]) -> &[Value] {
        tracing::debug!("calling function: {}", func_name);
        tracing::debug!("function args count: {}", args.len());

        let func_id = *self
            .builtin_funcs
            .get(func_name)
            .unwrap_or_else(|| panic!("Function '{}' not found in builtin_funcs", func_name));

        tracing::debug!("function id: {:?}", func_id);

        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let inst = self.builder.ins().call(func_ref, args);
        let results = self.builder.inst_results(inst);

        tracing::debug!(
            "function '{}' returned {} results",
            func_name,
            results.len()
        );

        results
    }

    fn bind_argument(&mut self, arg: &Arg, initial: Value) -> Value {
        let var = self.builder.declare_var(types::I64);
        self.locals.push((arg.clone(), var));

        let value = if arg.is_shared() {
            self.call_internal("create_indirect_object", &[initial])[0]
        } else {
            initial
        };

        self.builder.def_var(var, value);

        let sym = Symbol::from(arg.ident);
        let sym = self.translate_lispobj(&LispSymbol::from(sym));
        self.call_internal("bind_special", &[sym, value, self.env]);

        value
    }

    fn load_arg_from_ptr(&mut self, base: Value, index: usize) -> Value {
        self.builder
            .ins()
            .load(types::I64, MemFlags::new(), base, (index * 8) as i32)
    }

    fn setup_locals(&mut self, lambda: &Lambda, incoming: IncomingArgs<'_>) -> CodegenResult<()> {
        let captures = lambda.captures.borrow();
        let args = &lambda.args;

        self.call_internal("mark_bind", &[self.env]);

        let optional_args = args
            .optional
            .as_ref()
            .map(|vec| vec.as_slice())
            .unwrap_or(&[]);
        let normal_count = args.normal.len();

        match incoming {
            IncomingArgs::Trampoline { args_ptr, args_cnt } => {
                for (i, arg) in args.normal.iter().enumerate() {
                    let val = self.load_arg_from_ptr(args_ptr, i);
                    self.bind_argument(arg, val);
                }

                for (offset, arg) in optional_args.iter().enumerate() {
                    let idx = normal_count + offset;
                    let idx_val = self.builder.ins().iconst(types::I64, idx as i64);
                    let cond = self
                        .builder
                        .ins()
                        .icmp(IntCC::SignedLessThan, idx_val, args_cnt);

                    let present_block = self.builder.create_block();
                    let absent_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();
                    self.builder.append_block_param(merge_block, types::I64);

                    self.builder
                        .ins()
                        .brif(cond, present_block, &[], absent_block, &[]);

                    self.builder.switch_to_block(present_block);
                    self.builder.seal_block(present_block);
                    let val = self.load_arg_from_ptr(args_ptr, idx);
                    self.builder.ins().jump(merge_block, &[val.into()]);

                    self.builder.switch_to_block(absent_block);
                    self.builder.seal_block(absent_block);
                    let nil_val = self.nil();
                    self.builder.ins().jump(merge_block, &[nil_val.into()]);

                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);
                    let opt_val = self.builder.block_params(merge_block)[0];
                    self.bind_argument(arg, opt_val);
                }

                if let Some(rest_arg) = args.rest.as_ref() {
                    let rest_start = normal_count + optional_args.len();
                    let start_val = self.builder.ins().iconst(types::I64, rest_start as i64);
                    let cond = self
                        .builder
                        .ins()
                        .icmp(IntCC::SignedLessThan, start_val, args_cnt);

                    let present_block = self.builder.create_block();
                    let absent_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();
                    self.builder.append_block_param(merge_block, types::I64);

                    self.builder
                        .ins()
                        .brif(cond, present_block, &[], absent_block, &[]);

                    self.builder.switch_to_block(present_block);
                    self.builder.seal_block(present_block);

                    let rest_ptr = self.builder.ins().iadd(args_ptr, start_val);
                    let rest_cnt = self.builder.ins().isub(args_cnt, start_val);
                    let rest_val =
                        self.call_internal("collect_rest_args", &[rest_ptr, rest_cnt])[0];
                    self.builder.ins().jump(merge_block, &[rest_val.into()]);

                    self.builder.switch_to_block(absent_block);
                    self.builder.seal_block(absent_block);
                    let nil_val = self.nil();
                    self.builder.ins().jump(merge_block, &[nil_val.into()]);

                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);
                    let rest_value = self.builder.block_params(merge_block)[0];
                    self.bind_argument(rest_arg, rest_value);
                }
            }
            IncomingArgs::Direct { args: values } => {
                for (i, arg) in args.normal.iter().enumerate() {
                    let val = values
                        .get(i)
                        .copied()
                        .ok_or(CodegenError::InvalidArgFormat)?;
                    self.bind_argument(arg, val);
                }

                for (offset, arg) in optional_args.iter().enumerate() {
                    let idx = normal_count + offset;
                    let val = values.get(idx).copied().unwrap_or_else(|| self.nil());
                    self.bind_argument(arg, val);
                }

                if let Some(_) = args.rest.as_ref() {
                    // TODO should return CodegenError here?
                    self.builder.ins().trap(TrapCode::user(2).unwrap());
                }
            }
        }

        // actual value of captures are loaded afterwards
        for capture in captures.iter() {
            let var = self.builder.declare_var(types::I64);
            let ident = Ident::from(*capture);
            let arg = Arg::new_uncap(ident);
            self.locals.push((arg, var));
        }

        Ok(())
    }

    pub fn load_symbol<'s>(
        &mut self,
        symbol: Symbol,
        load_function_cell: bool,
    ) -> CodegenResult<Value> {
        tracing::debug!(
            "Loading symbol: {} (function_cell: {})",
            symbol.name(),
            load_function_cell
        );

        let is_function: i64 = if load_function_cell { 1 } else { 0 };
        let is_function = self.builder.ins().iconst(types::I64, is_function);
        let sym = self.translate_lispobj(&LispSymbol::from(symbol));
        let result = self.call_internal("load_symbol_value", &[sym, is_function, self.env])[0];

        Ok(result)
    }

    fn translate_lispobj<T: Tagged>(&mut self, obj: &T) -> Value {
        self.builder
            .ins()
            .iconst(types::I64, unsafe { obj.to_raw() } as i64)
    }

    fn translate_arg_exprs<'s>(&mut self, exprs: &[Expr]) -> CodegenResult<Vec<Value>> {
        exprs.iter().map(|e| self.translate_expr(e)).collect()
    }

    pub fn translate_expr<'s>(&mut self, expr: &Expr) -> CodegenResult<Value> {
        match &*expr.ty() {
            ExprType::Nil => Ok(self.nil()),
            ExprType::Symbol(var) => {
                match var.get() {
                    Var::Global(symbol) => {
                        let val = self.load_symbol(symbol, false)?;
                        Ok(val)
                    }
                    Var::Local(ident) | Var::Captured(ident) | Var::Argument(ident) => {
                        // All local variables (including captured and arguments) are stored in self.locals
                        let arg = Arg::new_uncap(ident);
                        if let Some(variable) = get_locals(&self.locals, &arg) {
                            let value = self.builder.use_var(variable);
                            Ok(value)
                        } else {
                            Err(CodegenError::SymbolNotFound(ident.into()))
                        }
                    }
                    Var::Unresolved(_) => {
                        unreachable!("Unresolved variable should not reach codegen")
                    }
                }
            }
            ExprType::Literal(literal) => self.translate_literal(literal),
            ExprType::Vector(exprs) => self.translate_vector(exprs),
            ExprType::Call(call) => self.translate_call(call),
            ExprType::SpecialForm(special_form) => self.translate_special_form(special_form),
        }
    }

    fn translate_literal(&mut self, literal: &Literal) -> CodegenResult<Value> {
        match literal {
            Literal::Number(Number::Integer(n)) => Ok(self.translate_lispobj(n)),
            Literal::Number(Number::Real(f)) => Ok(self.translate_lispobj(f)),
            Literal::Character(c) => Ok(self.translate_lispobj(c)),
            Literal::String(s) => Ok(self.translate_lispobj(s)),
        }
    }

    fn translate_vector<'s>(&mut self, exprs: &[Expr]) -> CodegenResult<Value> {
        // TODO: implement vector creation
        todo!("Vector creation not yet implemented")
    }

    fn translate_call<'s>(&mut self, call: &Call) -> CodegenResult<Value> {
        tracing::debug!(
            "Translating function call with {} arguments",
            call.args.len()
        );

        let func = self.load_symbol(call.symbol.get().into(), true)?;
        let args = self.translate_arg_exprs(&call.args)?;
        let argc = args.len();
        let argc_val = self.builder.ins().iconst(types::I64, argc as i64);

        // Check function arguments and get dispatch info
        let dispatch_info = self.call_internal("check_function_args", &[func, argc_val])[0];

        // Create blocks for different dispatch paths
        let error_block = self.builder.create_block();
        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        // Check for error (-1)
        let minus_one = self.builder.ins().iconst(types::I64, -1);
        let is_error = self
            .builder
            .ins()
            .icmp(IntCC::Equal, dispatch_info, minus_one);

        self.builder
            .ins()
            .brif(is_error, error_block, &[], call_block, &[]);

        // Error block - invalid argument count
        self.builder.switch_to_block(error_block);
        self.builder.seal_block(error_block);
        let error_result = self.call_internal("signal_wrong_number_of_args", &[func, argc_val])[0];
        self.builder.ins().jump(merge_block, &[error_result.into()]);

        // Call block - determine call type and execute
        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);

        // Check if we should use trampoline (0xffff) or direct call
        let trampoline_marker = self.builder.ins().iconst(types::I64, 0xffff);
        let is_trampoline = self
            .builder
            .ins()
            .icmp(IntCC::Equal, dispatch_info, trampoline_marker);

        let trampoline_block = self.builder.create_block();
        let direct_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_trampoline, trampoline_block, &[], direct_block, &[]);

        // Trampoline call block
        self.builder.switch_to_block(trampoline_block);
        self.builder.seal_block(trampoline_block);
        let trampoline_result = self.generate_trampoline_call(&args, func)?;
        self.builder
            .ins()
            .jump(merge_block, &[trampoline_result.into()]);

        // Direct call block
        self.builder.switch_to_block(direct_block);
        self.builder.seal_block(direct_block);
        let direct_result = self.generate_direct_call(&args, func, dispatch_info)?;
        self.builder
            .ins()
            .jump(merge_block, &[direct_result.into()]);

        // Merge block
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let result = self.builder.block_params(merge_block)[0];

        tracing::debug!("Function call completed successfully");
        Ok(result)
    }

    fn generate_direct_call(
        &mut self,
        args: &[Value],
        func: Value,
        dispatch_info: Value,
    ) -> CodegenResult<Value> {
        tracing::debug!(
            "Generating direct call with {} provided arguments",
            args.len()
        );

        // Resolve the function pointer once and reuse via an SSA var.
        let func_ptr = self.call_internal("get_func_ptr", &[func, self.env])[0];
        let func_ptr_var = self.builder.declare_var(types::I64);
        self.builder.def_var(func_ptr_var, func_ptr);

        // Prepare jump-table targets for argument counts 0..=8.
        let mut blocks = Vec::with_capacity(9);
        let mut block_calls = Vec::with_capacity(9);
        for _ in 0..=8 {
            let block = self.builder.create_block();
            let block_call = BlockCall::new(block, [], &mut self.builder.func.dfg.value_lists);
            block_calls.push(block_call);

            blocks.push(block);
        }

        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        let default_block = self.builder.create_block();
        let default_block_call =
            BlockCall::new(default_block, [], &mut self.builder.func.dfg.value_lists);
        let jump_table_data = JumpTableData::new(default_block_call, &block_calls);

        // Jump-table indices are limited to i32 in Cranelift.
        let dispatch_index = self.builder.ins().ireduce(types::I32, dispatch_info);
        let jump_table = self.builder.create_jump_table(jump_table_data);
        self.builder.ins().br_table(dispatch_index, jump_table);

        // Macro to generate call blocks with specific argument counts
        macro_rules! generate_call_block {
            ($block:expr, $argc:expr) => {{
                self.builder.switch_to_block($block);
                self.builder.seal_block($block);

                let mut sig = self.module.make_signature();
                for _ in 0..$argc {
                    sig.params.push(AbiParam::new(types::I64));
                }
                sig.params.push(AbiParam::new(types::I64)); // env
                sig.returns.push(AbiParam::new(types::I64));

                let sig_ref = self.builder.import_signature(sig);
                let func_ptr = self.builder.use_var(func_ptr_var);

                let mut call_args = Vec::with_capacity($argc + 1);
                let actual_len = args.len();
                for idx in 0..$argc {
                    let val = if idx < actual_len {
                        args[idx]
                    } else {
                        self.nil()
                    };
                    call_args.push(val);
                }
                call_args.push(self.env);

                let inst = self
                    .builder
                    .ins()
                    .call_indirect(sig_ref, func_ptr, &call_args);
                let result = self.builder.inst_results(inst)[0];

                self.builder.ins().jump(merge_block, &[result.into()]);
            }};
        }

        generate_call_block!(blocks[0], 0);
        generate_call_block!(blocks[1], 1);
        generate_call_block!(blocks[2], 2);
        generate_call_block!(blocks[3], 3);
        generate_call_block!(blocks[4], 4);
        generate_call_block!(blocks[5], 5);
        generate_call_block!(blocks[6], 6);
        generate_call_block!(blocks[7], 7);
        generate_call_block!(blocks[8], 8);

        self.builder.switch_to_block(default_block);
        self.builder.seal_block(default_block);
        self.builder.ins().trap(TrapCode::user(1).unwrap());

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let result = self.builder.block_params(merge_block)[0];

        Ok(result)
    }

    fn generate_trampoline_call(&mut self, args: &[Value], func: Value) -> CodegenResult<Value> {
        tracing::debug!("Generating trampoline call with {} arguments", args.len());

        let argc = args.len();

        // Create stack slot for arguments
        let slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: 8 * argc as u32,
            align_shift: 0,
        });

        // Store arguments to stack
        for (i, val) in args.iter().enumerate() {
            tracing::debug!("Storing argument {} to stack", i);
            self.builder.ins().stack_store(*val, slot, i as i32 * 8);
        }

        let args_ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
        let args_cnt = self.builder.ins().iconst(types::I64, argc as i64);

        // Create trampoline signature (args_ptr, args_cnt, env)
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64)); // args_ptr
        sig.params.push(AbiParam::new(types::I64)); // args_cnt
        sig.params.push(AbiParam::new(types::I64)); // env
        sig.returns.push(AbiParam::new(types::I64));

        let sig_ref = self.builder.import_signature(sig);
        let func_ptr = self.call_internal("get_func_ptr", &[func, self.env])[0];

        let inst =
            self.builder
                .ins()
                .call_indirect(sig_ref, func_ptr, &[args_ptr, args_cnt, self.env]);
        let result = self.builder.inst_results(inst)[0];

        Ok(result)
    }

    fn translate_special_form<'s>(&mut self, special_form: &SpecialForm) -> CodegenResult<Value> {
        match special_form {
            SpecialForm::If(if_expr) => self.translate_if_expr(if_expr),
            SpecialForm::Let(let_expr) => self.translate_let_expr(let_expr),
            SpecialForm::Lambda(lambda) => self.translate_lambda_expr(lambda),
            SpecialForm::Quote(quote) => self.translate_quote(quote),
            SpecialForm::Progn(exprs) => self.translate_progn(exprs),
            SpecialForm::And(exprs) => self.translate_and(exprs),
            SpecialForm::Or(exprs) => self.translate_or(exprs),
            SpecialForm::Defvar(exprs) => self.translate_defvar(exprs),
            _ => todo!("Special form not yet implemented: {:?}", special_form),
        }
    }

    fn translate_if_expr<'s>(&mut self, if_expr: &If) -> CodegenResult<Value> {
        let cond_val = self.translate_expr(&if_expr.cond)?;
        let nil = self.nil();
        let cond = self.builder.ins().icmp(IntCC::NotEqual, cond_val, nil);
        // let cond = cond_val;
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, types::I64);

        self.builder
            .ins()
            .brif(cond, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = self.translate_expr(&if_expr.then)?;
        self.builder.ins().jump(merge_block, &[then_return.into()]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_return = self.translate_progn(&if_expr.els)?;
        self.builder.ins().jump(merge_block, &[else_return.into()]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let phi = self.builder.block_params(merge_block)[0];
        Ok(phi)
    }

    fn translate_and<'s>(&mut self, exprs: &[Expr]) -> CodegenResult<Value> {
        if exprs.is_empty() {
            return Ok(self.t()); // true
        }

        // Create an end block to jump to when we want to exit early
        let end_block = self.builder.create_block();
        self.builder.append_block_param(end_block, types::I64);

        // Create blocks for each expression
        let blocks = exprs
            .iter()
            .map(|_| self.builder.create_block())
            .collect::<Vec<_>>();

        // Start with the first expression
        self.builder.switch_to_block(blocks[0]);
        self.builder.seal_block(blocks[0]);

        let first_val = self.translate_expr(&exprs[0])?;
        let nil = self.nil();

        if exprs.len() == 1 {
            self.builder.ins().jump(end_block, &[first_val.into()]);
            self.builder.switch_to_block(end_block);
            self.builder.seal_block(end_block);
            let result = self.builder.block_params(end_block)[0];
            return Ok(result);
        }

        // Check if first value is nil
        let cond = self.builder.ins().icmp(IntCC::NotEqual, first_val, nil);
        self.builder
            .ins()
            .brif(cond, blocks[1], &[], end_block, &[nil.into()]);

        // Process remaining expressions
        for i in 1..exprs.len() {
            self.builder.switch_to_block(blocks[i]);
            self.builder.seal_block(blocks[i]);

            let expr_val = self.translate_expr(&exprs[i])?;

            if i == exprs.len() - 1 {
                // Last expression, jump to end with its value
                self.builder.ins().jump(end_block, &[expr_val.into()]);
            } else {
                // Middle expression, check for nil
                let cond = self.not_nil(expr_val);
                self.builder
                    .ins()
                    .brif(cond, blocks[i + 1], &[], end_block, &[nil.into()]);
            }
        }

        // End block
        self.builder.switch_to_block(end_block);
        self.builder.seal_block(end_block);
        let result = self.builder.block_params(end_block)[0];
        Ok(result)
    }

    fn translate_or<'s>(&mut self, exprs: &[Expr]) -> CodegenResult<Value> {
        if exprs.is_empty() {
            return Ok(self.nil()); // nil
        }

        // Create an end block to jump to when we want to exit early (with a non-nil value)
        let end_block = self.builder.create_block();
        self.builder.append_block_param(end_block, types::I64);

        // Create blocks for each expression
        let blocks = exprs
            .iter()
            .map(|_| self.builder.create_block())
            .collect::<Vec<_>>();

        // Start with the first expression
        self.builder.switch_to_block(blocks[0]);
        self.builder.seal_block(blocks[0]);

        let first_val = self.translate_expr(&exprs[0])?;
        let nil = self.nil();

        if exprs.len() == 1 {
            self.builder.ins().jump(end_block, &[first_val.into()]);
            self.builder.switch_to_block(end_block);
            self.builder.seal_block(end_block);
            let result = self.builder.block_params(end_block)[0];
            return Ok(result);
        }

        // Check if first value is not nil
        let cond = self.not_nil(first_val);
        self.builder
            .ins()
            .brif(cond, end_block, &[first_val.into()], blocks[1], &[]);

        // Process remaining expressions
        for i in 1..exprs.len() {
            self.builder.switch_to_block(blocks[i]);
            self.builder.seal_block(blocks[i]);

            let expr_val = self.translate_expr(&exprs[i])?;

            if i == exprs.len() - 1 {
                // Last expression, jump to end with its value (even if nil)
                self.builder.ins().jump(end_block, &[expr_val.into()]);
            } else {
                // Middle expression, check if not nil
                let cond = self.builder.ins().icmp(IntCC::NotEqual, expr_val, nil);
                self.builder
                    .ins()
                    .brif(cond, end_block, &[expr_val.into()], blocks[i + 1], &[]);
            }
        }

        // End block
        self.builder.switch_to_block(end_block);
        self.builder.seal_block(end_block);
        let result = self.builder.block_params(end_block)[0];
        Ok(result)
    }

    fn translate_quote<'s>(&mut self, quote: &Quote) -> CodegenResult<Value> {
        match quote.kind {
            QuoteKind::Quote => self.translate_quoted_data(&quote.expr),
            _ => {
                // TODO: implement backquote with unquote support
                todo!("Backquote not yet implemented")
            }
        }
    }

    fn translate_quoted_data(&mut self, data: &QuotedData) -> CodegenResult<Value> {
        match data {
            QuotedData::Literal(literal) => self.translate_literal(literal),
            QuotedData::Symbol(ident) => {
                let symbol: Symbol = ident.get().into();
                Ok(self.translate_lispobj(&LispSymbol::from(symbol)))
            }
            QuotedData::List(_items) => {
                // TODO: implement list creation
                todo!("Quoted list creation not yet implemented")
            }
            QuotedData::Vector(_items) => {
                // TODO: implement vector creation
                todo!("Quoted vector creation not yet implemented")
            }
            QuotedData::Unquote(_) | QuotedData::UnquoteSplice(_) => {
                Err(CodegenError::InvalidUnquote)
            }
        }
    }

    fn translate_lambda_expr<'s>(&mut self, lambda: &Lambda) -> CodegenResult<Value> {
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();

        let mut codegen = Codegen::new(
            self.module,
            self.data_desc,
            self.builtin_funcs,
            &mut fctx,
            &mut ctx,
            &lambda,
        )?;

        let result = codegen.translate_exprs(&lambda.body.body)?;
        codegen.call_internal("unbind_special", &[codegen.env]);

        let func_id = codegen.func_id;
        let (result, unresolved) = codegen.finalize(result);

        tracing::debug!("Defining lambda function with id: {:?}", func_id);
        self.module.define_function(func_id, &mut ctx)?;
        self.module.clear_context(&mut ctx);

        self.module.finalize_definitions()?;
        let func_ptr = self.module.get_finalized_function(func_id);
        result.func.set_func_ptr(func_ptr);
        let func_val = self.translate_lispobj(&result.func);

        // resolve closure captures
        let result = result.resolve(self);
        self.defined_funcs.push(result);

        for func in unresolved.into_iter() {
            if let Closure::Unresolved(func) = func {
                let result = func.resolve(self);
                self.defined_funcs.push(result);
            }
        }
        Ok(func_val)
    }

    fn translate_let_expr<'s>(&mut self, let_expr: &Let) -> CodegenResult<Value> {
        // Remember the current locals count to restore later
        let locals_before = self.locals.len();

        // Evaluate values in parent scope and create new variables
        let mut new_vars = Vec::new();
        self.call_internal("mark_bind", &[self.env]);
        for (arg, value_expr) in &*let_expr.bindings {
            // Evaluate the value expression in the *parent* scope (before adding the new variable)
            let mut value = if let Some(expr) = value_expr {
                self.translate_expr(expr)?
            } else {
                self.nil()
            };

            // Create the variable and add to locals
            let var = self.builder.declare_var(types::I64);

            // Check value's type, if it is Indirect, then directly load it.
            // If arg is shared and value is not Indirect, make it indirect.
            if arg.is_shared() {
                value = self.call_internal("create_indirect_object", &[value])[0];
            }

            self.builder.def_var(var, value);
            new_vars.push((arg.clone(), var));

            let sym = Symbol::from(arg.ident);
            let sym = self.translate_lispobj(&LispSymbol::from(sym));

            // let is_special = self.call_internal("is_special", &[sym, self.env])[0];
            // let fals = self.builder.ins().iconst(types::I64, 0);
            // let is_special = self.builder.ins().icmp(IntCC::Equal, is_special, fals);
            // let special_block = self.builder.create_block();

            self.call_internal("bind_special", &[sym, value, self.env]);
        }

        // Add all new variables to locals after all values are evaluated
        self.locals.extend(new_vars);

        // Translate body with new variables in scope
        let result = self.translate_exprs(&let_expr.body.body)?;

        // Pop the let-bound variables from locals
        self.locals.truncate(locals_before);
        self.call_internal("unbind_special", &[self.env]);

        Ok(result)
    }

    fn translate_defvar<'s>(&mut self, defvar_expr: &Defvar) -> CodegenResult<Value> {
        let text = defvar_expr.symbol.text().as_bytes();
        let text_len = text.len();
        let slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: 8 * text_len as u32,
            align_shift: 0,
        });
        for (i, val) in text.iter().enumerate() {
            let val = self.builder.ins().iconst(types::I8, *val as i64);
            self.builder.ins().stack_store(val, slot, i as i32 * 1);
        }
        let sym = self.builder.ins().stack_addr(types::I64, slot, 0);
        let sym_len = self.builder.ins().iconst(types::I64, text_len as i64);

        let val = self.translate_expr(defvar_expr.value.as_ref().unwrap())?;

        let val = self.call_internal("defvar", &[sym, sym_len, val, self.env])[0];

        Ok(val)
    }
}
