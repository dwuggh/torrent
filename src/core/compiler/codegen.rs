use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Result;
use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;

use crate::core::compiler::ir::{Expr, SpecialForm, Literal, Number, Call, Let, If, Lambda, Quote, QuoteKind, QuotedData};
use crate::core::compiler::error::{CodegenError, CodegenResult};
use crate::core::compiler::scope::CompileScope;
use crate::core::compiler::scope::FrameScope;
use crate::core::compiler::scope::Val;
use crate::core::env::Environment;
use crate::core::function::Function;
use crate::core::ident::Ident;
use crate::core::string::LispString;
use crate::core::symbol::Symbol;
use crate::core::value::LispValue;
use crate::core::value::TaggedPtr;
use crate::core::value::NIL;
use crate::core::value::TRUE;
use crate::Value as RuntimeValue;

pub struct Codegen<'a> {
    module: &'a mut JITModule,
    builder: FunctionBuilder<'a>,
    closure: Value,
    func: RuntimeValue,
    pub func_id: FuncId,
    builtin_funcs: &'a HashMap<String, FuncId>,
}

impl<'a> Codegen<'a> {
    pub fn new<'s>(
        module: &'a mut JITModule,
        builtin_funcs: &'a HashMap<String, FuncId>,
        fctx: &'a mut FunctionBuilderContext,
        ctx: &'a mut Context,
        args: &[Ident],
        parent_scope: &'s CompileScope,
    ) -> CodegenResult<(Self, CompileScope<'s>)> {
        // make signature

        let sig = &mut ctx.func.signature;
        for _ in 0..args.len() {
            sig.params.push(AbiParam::new(types::I64));
        }
        // NOTE env arg
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        let func_id = module.declare_anonymous_function(&ctx.func.signature)?;
        let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // builder.block_params(entry_block);

        let mut variables = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let sym = (*arg).into();
            let val = builder.block_params(entry_block)[i];
            let var = builder.declare_var(types::I64);
            variables.insert(sym, var);
            builder.def_var(var, val);
        }

        let new_scope = FrameScope::new(variables, parent_scope, false, true).into();

        let func_runtime_val = Function::new_closure(0 as *const u8, func_id).tag();
        let closure_val = translate_value(&mut builder, func_runtime_val);

        let codegen = Self {
            module,
            builtin_funcs,
            builder,
            func: func_runtime_val,
            closure: closure_val,
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

    pub fn translate_exprs<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        exprs
            .iter()
            .map(|e| self.translate_expr(e, scope))
            .last()
            .unwrap_or(Ok(self.nil()))
    }

    pub fn finalize(mut self, val: Value) -> RuntimeValue {
        self.builder.ins().return_(&[val]);
        self.builder.finalize();
        self.func
    }

    fn call(&mut self, func_name: &str, args: &[Value]) -> &[Value] {
        let func_id = *self.builtin_funcs.get(func_name).unwrap();
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let inst = self.builder.ins().call(func_ref, args);
        self.builder.inst_results(inst)
    }

    pub fn load_symbol<'s>(
        &mut self,
        scope: &CompileScope<'s>,
        symbol: Symbol,
        load_function_cell: bool,
    ) -> CodegenResult<Value> {
        scope
            .load_symbol(symbol, load_function_cell, self.func, &mut self.builder)
            .ok_or(CodegenError::SymbolNotFound(symbol))
            .map(|val| match val {
                Val::Value(value) => value,
                Val::Symbol(symbol) => {
                    // this value is captured, we load it from the map for captured values
                    // through compiler, so it gets loaded at runtime
                    let func_id = *self.builtin_funcs.get("--load-captured").unwrap();
                    let sym = translate_value(&mut self.builder, symbol.tag());

                    let load = self.module.declare_func_in_func(func_id, self.builder.func);
                    let inst = self.builder.ins().call(load, &[sym, self.closure]);
                    self.builder.inst_results(inst)[0]
                }
            })
    }

    fn translate_arg_exprs<'s>(
        &mut self,
        exprs: &[Expr],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Vec<Value>> {
        exprs
            .iter()
            .map(|e| self.translate_expr(e, scope))
            .collect()
    }

    pub fn translate_expr<'s>(
        &mut self,
        expr: &Expr,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        match expr {
            Expr::Symbol(ident) => {
                let symbol = (*ident).into();
                let val = self.load_symbol(scope, symbol, false)?;
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
            Literal::Number(Number::FixedInteger(n)) => {
                let val = RuntimeValue::tag(*n).0 as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Literal::Number(Number::Real(f)) => {
                let val = RuntimeValue::tag(*f).0 as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Literal::Character(c) => {
                let val = RuntimeValue::tag(*c).0 as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Literal::String(s) => {
                let val = RuntimeValue::tag(LispString::from_str(s)).0 as i64;
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
        // Check if it's a builtin function
        if let Expr::Symbol(fn_name) = call.func.as_ref() {
            let fn_name_str = fn_name.text();
            
            match fn_name_str {
                "+" => {
                    let args = self.translate_arg_exprs(&call.args, scope)?;
                    if args.len() >= 2 {
                        let res = self.builder.ins().iadd(args[0], args[1]);
                        return Ok(res);
                    }
                }
                _ => (), // Not a builtin, fall through to dynamic dispatch
            }
        }

        // Dynamic function call
        let func = self.translate_expr(call.func.as_ref(), scope)?;
        let args = self.translate_arg_exprs(&call.args, scope)?;

        let slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: 8 * args.len() as u32,
            align_shift: 0,
        });

        for (i, val) in args.iter().enumerate() {
            self.builder.ins().stack_store(*val, slot, i as i32 * 8);
        }

        let args_ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
        let args_len = self.builder.ins().iconst(types::I64, args.len() as i64);
        let env_ptr = scope.get_root() as *const Environment;
        let env_ptr = self.builder.ins().iconst(types::I64, env_ptr as i64);

        let res = self.call("apply", &[func, args_ptr, args_len, env_ptr])[0];
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
            _ => todo!("Special form not yet implemented: {:?}", special_form),
        }
    }

    fn translate_if_expr<'s>(
        &mut self,
        if_expr: &If,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        let cond_val = self.translate_expr(&if_expr.cond, scope)?;
        let nil = self.nil();
        let cond = self.builder.ins().icmp(IntCC::NotEqual, cond_val, nil);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, types::I64);

        self.builder
            .ins()
            .brif(cond, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = self.translate_expr(&if_expr.then, scope)?;
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

        let mut result = self.translate_expr(&exprs[0], scope)?;
        let nil = self.nil();

        for expr in &exprs[1..] {
            let continue_block = self.builder.create_block();
            let end_block = self.builder.create_block();
            self.builder.append_block_param(end_block, types::I64);

            let cond = self.builder.ins().icmp(IntCC::NotEqual, result, nil);
            self.builder
                .ins()
                .brif(cond, continue_block, &[], end_block, &[nil]);

            self.builder.switch_to_block(continue_block);
            self.builder.seal_block(continue_block);
            result = self.translate_expr(expr, scope)?;
            self.builder.ins().jump(end_block, &[result]);

            self.builder.switch_to_block(end_block);
            self.builder.seal_block(end_block);
            result = self.builder.block_params(end_block)[0];
        }

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

        let mut result = self.translate_expr(&exprs[0], scope)?;
        let nil = self.nil();

        for expr in &exprs[1..] {
            let continue_block = self.builder.create_block();
            let end_block = self.builder.create_block();
            self.builder.append_block_param(end_block, types::I64);

            let cond = self.builder.ins().icmp(IntCC::Equal, result, nil);
            self.builder
                .ins()
                .brif(cond, continue_block, &[], end_block, &[result]);

            self.builder.switch_to_block(continue_block);
            self.builder.seal_block(continue_block);
            result = self.translate_expr(expr, scope)?;
            self.builder.ins().jump(end_block, &[result]);

            self.builder.switch_to_block(end_block);
            self.builder.seal_block(end_block);
            result = self.builder.block_params(end_block)[0];
        }

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
                let symbol = (*ident).into();
                let val = RuntimeValue::tag(symbol).0 as i64;
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
        // TODO: implement lambda compilation
        todo!("Lambda compilation not yet implemented")
    }

    pub fn translate_defun<'s>(
        &mut self,
        scope: &CompileScope<'s>,
        args: &[Ident],
        body: &[Expr],
    ) -> CodegenResult<Function> {
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();

        let (mut codegen, new_scope) = Codegen::new(
            self.module,
            self.builtin_funcs,
            &mut fctx,
            &mut ctx,
            args,
            scope,
        )?;

        let result = codegen.translate_exprs(body, &new_scope)?;

        let func_id = codegen.func_id;
        let func = codegen.finalize(result);

        self.module.define_function(func_id, &mut ctx)?;
        let func_ptr = self.module.get_finalized_function(func_id);
        let closure: Function = func.try_into().unwrap();
        closure.set_func_ptr(func_ptr);

        self.module.clear_context(&mut ctx);
        Ok(closure)
    }


    fn translate_let_expr<'s>(
        &mut self,
        let_expr: &Let,
        parent_scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        let mut new_vars = HashMap::new();
        let mut binding_values = Vec::new();

        for (ident, value_expr) in &let_expr.bindings {
            let sym = (*ident).into();
            let var = self.builder.declare_var(types::I64);
            new_vars.insert(sym, var);

            // The values are evaluated in the *parent* scope.
            let value = if let Some(expr) = value_expr {
                self.translate_expr(expr, parent_scope)?
            } else {
                self.nil()
            };
            binding_values.push((sym, value));
        }

        let new_scope = FrameScope::new(new_vars, parent_scope, true, false).into();

        // Define the variables with their evaluated values
        if let CompileScope::Frame(frame) = &new_scope {
            for (sym, value) in binding_values {
                let var = frame.slots.get(sym).unwrap();
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
                for (symbol, funcs) in binds.borrow().iter() {
                    let var = frame.slots.get(*symbol).unwrap();
                    let value = self.builder.use_var(var);
                    let sym = translate_value(&mut self.builder, symbol.tag());
                    for func in funcs.iter() {
                        let func_val = translate_value(&mut self.builder, *func);
                        self.call("__store_lexical", &[sym, value, func_val]);
                    }
                }
            }
        }

        Ok(result)
    }
}

fn translate_value(builder: &mut FunctionBuilder, val: RuntimeValue) -> Value {
    builder.ins().iconst(types::I64, val.0 as i64)
}
