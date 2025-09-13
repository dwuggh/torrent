use std::collections::HashMap;

use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;

use crate::core::compiler::error::{CodegenError, CodegenResult};
use crate::core::compiler::ir::*;
use crate::core::compiler::scope::CompileScope;
use crate::core::compiler::scope::FrameScope;
use crate::core::compiler::scope::Val;
use crate::core::function::LispFunction;
use crate::core::string::LispStr;
use crate::core::symbol::Symbol;
use crate::core::object::NIL;
use crate::core::object::TRUE;
use crate::core::TaggedPtr;

pub struct Codegen<'a> {
    module: &'a mut JITModule,
    data_desc: &'a mut DataDescription,
    builder: FunctionBuilder<'a>,
    func: LispFunction,
    pub func_id: FuncId,
    env: Value,
    builtin_funcs: &'a HashMap<String, FuncId>,
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

        let func = LispFunction::new_closure(func_id);
        // let closure_val = translate_value(&mut builder, func_runtime_val);

        let codegen = Self {
            module,
            data_desc,
            builtin_funcs,
            builder,
            func,
            // closure: closure_val,
            env,
            func_id,
        };

        Ok(codegen)
    }

    pub fn new<'s>(
        module: &'a mut JITModule,
        data_desc: &'a mut DataDescription,
        builtin_funcs: &'a HashMap<String, FuncId>,
        fctx: &'a mut FunctionBuilderContext,
        ctx: &'a mut Context,
        args: &[Arg],
        parent_scope: &'s CompileScope,
    ) -> CodegenResult<(Self, CompileScope<'s>)> {
        tracing::debug!("Creating new codegen with {} args", args.len());

        // make signature
        let sig = &mut ctx.func.signature;
        // args_ptr: pointer to arguments array
        sig.params.push(AbiParam::new(types::I64));
        // args_cnt: number of arguments
        sig.params.push(AbiParam::new(types::I64));
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

        let args_ptr = block_params[0];
        let args_cnt = block_params[1];
        let env = block_params[2];

        let mut variables = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let arg = arg.ident;
            let sym = arg;
            let var = builder.declare_var(types::I64);
            variables.insert(sym, var);

            tracing::debug!("Loading argument {} ({})", i, arg.text());

            // Load argument from the arguments array
            let val = builder
                .ins()
                .load(types::I64, MemFlags::new(), args_ptr, (i * 8) as i32);
            builder.def_var(var, val);
        }

        let new_scope = FrameScope::new(variables, parent_scope, false, true).into();

        let func = LispFunction::new_closure(func_id);

        let codegen = Self {
            module,
            data_desc,
            builtin_funcs,
            builder,
            func,
            env,
            func_id,
        };

        Ok((codegen, new_scope))
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

    pub fn translate_exprs<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        // TODO need to drop other values
        exprs
            .iter()
            .map(|e| self.translate_expr(e, scope, false))
            .last()
            .unwrap_or(Ok(self.nil()))
    }

    pub fn finalize(mut self, val: Value) -> LispFunction {
        self.builder.ins().return_(&[val]);
        self.builder.finalize();
        self.func
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

    pub fn load_symbol<'s>(
        &mut self,
        scope: &CompileScope<'s>,
        symbol: Symbol,
        load_function_cell: bool,
    ) -> CodegenResult<Value> {
        tracing::debug!(
            "Loading symbol: {} (function_cell: {})",
            symbol.name.text(),
            load_function_cell
        );

        self.load_symbol_inner(scope, symbol, load_function_cell, true)
            .ok_or(CodegenError::SymbolNotFound(symbol))
            .map(|val| match val {
                Val::Value(value) => {
                    tracing::debug!("Symbol loaded as direct value");
                    value
                }
                Val::Ident(ident) => {
                    tracing::debug!("Symbol is captured, loading from closure: {}", ident.text());
                    // this value is captured, we load it from the map for captured values
                    // through compiler, so it gets loaded at runtime
                    let ident_val = self
                        .builder
                        .ins()
                        .iconst(types::I64, Into::<i64>::into(ident));

                    let closure = self
                        .builder
                        .ins()
                        .iconst(types::I64, self.func.raw() as i64);

                    self.call_internal("load_captured", &[ident_val, closure, self.env])[0]
                }
            })
    }

    fn load_symbol_inner<'s>(
        &mut self,
        scope: &CompileScope<'s>,
        symbol: Symbol,
        load_function_cell: bool,
        same_func_scope: bool,
    ) -> Option<Val> {
        match scope {
            CompileScope::Global => {
                // let val = Environment::default().load_symbol(symbol, load_function_cell)?;
                let sym_val = self.builder.ins().iconst(types::I64, symbol.raw() as i64);
                let load_function_cell = self
                    .builder
                    .ins()
                    .iconst(types::I64, if load_function_cell { 1 } else { 0 });
                let val = self.call_internal(
                    "load_symbol_value",
                    &[sym_val, load_function_cell, self.env],
                )[0];
                Some(Val::Value(val))
            }
            CompileScope::Frame(frame) => match frame.slots.get(symbol.name) {
                Some(var) => {
                    if same_func_scope {
                        Some(Val::Value(self.builder.use_var(var)))
                    } else {
                        if let Some(lexical_binds) = frame.lexical_binds.as_ref() {
                            if let Some(captured) =
                                lexical_binds.borrow_mut().get_mut(&symbol.into())
                            {
                                captured.push(self.func.clone());
                            }
                        }
                        Some(Val::Ident(symbol.into()))
                    }
                }
                None => self.load_symbol_inner(
                    frame.parent,
                    symbol,
                    load_function_cell,
                    same_func_scope && frame.is_func,
                ),
            },
        }
    }

    fn translate_arg_exprs<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Vec<Value>> {
        exprs
            .iter()
            .map(|e| self.translate_expr(e, scope, false))
            .collect()
    }

    pub fn translate_expr<'s>(
        &mut self,
        expr: &Expr,
        scope: &CompileScope<'s>,
        load_function_cell: bool,
    ) -> CodegenResult<Value> {
        match expr {
            Expr::Nil => Ok(self.nil()),
            Expr::Symbol(ident) => {
                let symbol = (*ident).into();
                let val = self.load_symbol(scope, symbol, load_function_cell)?;
                Ok(val)
            }
            Expr::Literal(literal) => self.translate_literal(literal),
            Expr::Vector(exprs) => self.translate_vector(exprs, scope),
            Expr::Call(call) => self.translate_call(call, scope),
            Expr::SpecialForm(special_form) => self.translate_special_form(special_form, scope),
        }
    }

    fn translate_literal(&mut self, literal: &Literal) -> CodegenResult<Value> {
        match literal {
            Literal::Number(Number::Integer(n)) => {
                let val = n.raw() as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Literal::Number(Number::Real(f)) => {
                let val = f.raw() as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Literal::Character(c) => {
                let val = c.raw() as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Literal::String(s) => {
                let val = s.raw() as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
        }
    }

    fn translate_vector<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        // TODO: implement vector creation
        todo!("Vector creation not yet implemented")
    }

    fn translate_call<'s>(
        &mut self,
        call: &Call,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        tracing::debug!(
            "Translating function call with {} arguments",
            call.args.len()
        );
        // match *call.func {
        //     Expr::Symbol(ident) => {
        //         match ident.text() {
        //             "+" => {
        //                 let l = &call.args[0];
        //                 let r = &call.args[1];
        //                 let l = self.translate_expr(l, scope, false)?;
        //                 let r = self.translate_expr(r, scope, false)?;
        //                 let res = self.builder.ins().iadd(l, r);
        //                 return Ok(res);
        //             }
        //             "-" => {
        //                 let l = &call.args[0];
        //                 let r = &call.args[1];
        //                 let l = self.translate_expr(l, scope, false)?;
        //                 let r = self.translate_expr(r, scope, false)?;
        //                 let res = self.builder.ins().isub(l, r);
        //                 return Ok(res);
        //             }
        //             "<" => {
        //                 let l = &call.args[0];
        //                 let r = &call.args[1];
        //                 let l = self.translate_expr(l, scope, false)?;
        //                 let r = self.translate_expr(r, scope, false)?;
        //                 let res = self.builder.ins().icmp(IntCC::SignedLessThan, l, r);
        //                 return Ok(res);
        //             }
        //             _ => ()
        //         }
        //     }
        //     _ => ()
        // };

        let func = self.translate_expr(call.func.as_ref(), scope, true)?;
        let args = self.translate_arg_exprs(&call.args, scope)?;
        let argc = args.len();

        tracing::debug!("Creating stack slot for {} arguments", args.len());
        let slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: 8 * argc as u32,
            align_shift: 0,
        });

        // self.builder.ins().stack_store(func, slot, 0);
        for (i, val) in args.iter().enumerate() {
            tracing::debug!("Storing argument {} to stack", i);
            self.builder.ins().stack_store(*val, slot, i as i32 * 8);
        }

        let args_ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
        let args_cnt = self.builder.ins().iconst(types::I64, argc as i64);

        // tracing::debug!("Calling apply function");
        // let res = self.call_internal("apply", &[func, args_ptr, args_cnt, self.env])[0];
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        let sig_ref = self.builder.import_signature(sig);
        let func_ptr = self.call_internal("get_func_ptr", &[func, self.env])[0];
        let inst = self.builder.ins().call_indirect(sig_ref, func_ptr, &[args_ptr, args_cnt, self.env]);
        let res = self.builder.inst_results(inst)[0];
        tracing::debug!("Apply function returned successfully");
        Ok(res)
    }

    fn translate_special_form<'s>(
        &mut self,
        special_form: &SpecialForm,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        match special_form {
            SpecialForm::If(if_expr) => self.translate_if_expr(if_expr, scope),
            SpecialForm::Let(let_expr) => self.translate_let_expr(let_expr, scope),
            SpecialForm::Lambda(lambda) => self.translate_lambda_expr(lambda, scope),
            SpecialForm::Quote(quote) => self.translate_quote(quote, scope),
            SpecialForm::Progn(exprs) => self.translate_exprs(exprs, scope),
            SpecialForm::And(exprs) => self.translate_and(exprs, scope),
            SpecialForm::Or(exprs) => self.translate_or(exprs, scope),
            SpecialForm::Defvar(exprs) => self.translate_defvar(exprs, scope),
            _ => todo!("Special form not yet implemented: {:?}", special_form),
        }
    }

    fn translate_if_expr<'s>(
        &mut self,
        if_expr: &If,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        let cond_val = self.translate_expr(&if_expr.cond, scope, false)?;
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
        let then_return = self.translate_expr(&if_expr.then, scope, false)?;
        self.builder.ins().jump(merge_block, &[then_return.into()]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_return = self.translate_exprs(&if_expr.els, scope)?;
        self.builder.ins().jump(merge_block, &[else_return.into()]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let phi = self.builder.block_params(merge_block)[0];
        Ok(phi)
    }

    fn translate_and<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
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

        let first_val = self.translate_expr(&exprs[0], scope, false)?;
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

            let expr_val = self.translate_expr(&exprs[i], scope, false)?;

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

    fn translate_or<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
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

        let first_val = self.translate_expr(&exprs[0], scope, false)?;
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

            let expr_val = self.translate_expr(&exprs[i], scope, false)?;

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

    fn translate_quote<'s>(
        &mut self,
        quote: &Quote,
        _scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        match quote.kind {
            QuoteKind::Quote => self.translate_quoted_data(&quote.expr),
            QuoteKind::Backquote => {
                // TODO: implement backquote with unquote support
                todo!("Backquote not yet implemented")
            }
        }
    }

    fn translate_quoted_data(&mut self, data: &QuotedData) -> CodegenResult<Value> {
        match data {
            QuotedData::Literal(literal) => self.translate_literal(literal),
            QuotedData::Symbol(ident) => {
                let symbol: Symbol = (*ident).into();
                let val = symbol.raw() as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
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

    fn translate_lambda_expr<'s>(
        &mut self,
        lambda: &Lambda,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        tracing::info!(
            "Translating lambda with {} args: {lambda:?}",
            lambda.args.len()
        );
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();

        let (mut codegen, new_scope) = Codegen::new(
            self.module,
            self.data_desc,
            self.builtin_funcs,
            &mut fctx,
            &mut ctx,
            &lambda.args,
            scope,
        )?;

        tracing::debug!(
            "Translating lambda body with {} expressions",
            lambda.body.len()
        );
        let result = codegen.translate_exprs(&lambda.body, &new_scope)?;

        let func_id = codegen.func_id;
        // TODO make sure this is still valid after storing func_ptr
        let func = codegen.finalize(result);

        tracing::debug!("Defining lambda function with id: {:?}", func_id);
        self.module.define_function(func_id, &mut ctx)?;
        self.module.clear_context(&mut ctx);

        tracing::debug!("Finalizing lambda function definitions");
        self.module.finalize_definitions()?;
        let func_ptr = self.module.get_finalized_function(func_id);
        func.set_func_ptr(func_ptr);

        let func_val = self.builder.ins().iconst(types::I64, func.raw() as i64);
        Ok(func_val)
    }

    fn translate_let_expr<'s>(
        &mut self,
        let_expr: &Let,
        parent_scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        let mut new_vars = HashMap::new();
        let mut binding_values = Vec::new();

        for (ident, value_expr) in &let_expr.bindings {
            let var = self.builder.declare_var(types::I64);
            new_vars.insert(*ident, var);

            // The values are evaluated in the *parent* scope.
            let value = if let Some(expr) = value_expr {
                self.translate_expr(expr, parent_scope, false)?
            } else {
                self.nil()
            };
            binding_values.push((ident, value));
        }

        let new_scope = FrameScope::new(new_vars, parent_scope, true, false).into();

        // Define the variables with their evaluated values
        if let CompileScope::Frame(frame) = &new_scope {
            for (sym, value) in binding_values {
                let var = frame.slots.get(*sym).unwrap();
                self.builder.def_var(var, value);
            }
        } else {
            unreachable!();
        }

        // translate body
        let result = self.translate_exprs(&let_expr.body, &new_scope)?;

        // resolve bindings for closures capturing let-bound variables
        if let CompileScope::Frame(frame) = &new_scope {
            if let Some(binds) = &frame.lexical_binds {
                for (ident, funcs) in binds.borrow().iter() {
                    let var = frame.slots.get(*ident).unwrap();
                    let value = self.builder.use_var(var);
                    let symbol: Symbol = ident.into();
                    let sym = self.builder.ins().iconst(types::I64, symbol.raw() as i64);
                    for func in funcs.iter() {
                        let func_val = self.builder.ins().iconst(types::I64, func.raw() as i64);
                        self.call_internal("store_captured", &[sym, value, func_val]);
                    }
                }
            }
        }

        Ok(result)
    }

    fn translate_defvar<'s>(
        &mut self,
        defvar_expr: &Defvar,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
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

        let val = self.translate_expr(defvar_expr.value.as_ref().unwrap(), scope, false)?;

        let val = self.call_internal("defvar", &[sym, sym_len, val, self.env])[0];

        Ok(val)
    }
}
