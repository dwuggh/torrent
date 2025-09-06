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

use crate::ast::Node;
use crate::core::compiler::error::{CodegenError, CodegenResult};
use crate::core::compiler::scope::CompileScope;
use crate::core::compiler::scope::FrameScope;
use crate::core::compiler::scope::Val;
use crate::core::env::Environment;
use crate::core::function::Function;
use crate::core::ident::Ident;
use crate::core::string::LispString;
use crate::core::symbol::Symbol;
use crate::core::value::TaggedPtr;
use crate::core::value::NIL;
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
        args: &[Node],
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
            let Node::Ident(arg) = arg else {
                return Err(CodegenError::InvalidArgFormat);
            };
            let sym = arg.into();
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

    pub fn translate_nodes<'s>(
        &mut self,
        nodes: &[Node],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        nodes
            .iter()
            .map(|n| self.translate_node(n, scope))
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

    fn translate_arg_nodes<'s>(
        &mut self,
        nodes: &[Node],
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Vec<Value>> {
        nodes
            .iter()
            .map(|n| self.translate_node(n, scope))
            .collect()
    }

    pub fn translate_node<'s>(
        &mut self,
        node: &Node,
        scope: &CompileScope<'s>,
    ) -> CodegenResult<Value> {
        match node {
            Node::Ident(ident) => {
                let symbol = ident.into();
                let val = self.load_symbol(scope, symbol, false)?;
                Ok(val)
            }
            Node::Sexp(nodes) => {
                if nodes.is_empty() {
                    return Ok(self.builder.ins().iconst(types::I64, 0));
                }

                let head = &nodes[0];
                let arg_nodes = &nodes[1..];

                if let Node::Ident(fn_name) = head {
                    let fn_name_str = fn_name.text();

                    // Handle special forms that don't evaluate all their arguments upfront
                    match fn_name_str {
                        "let" => {
                            return self.translate_let(scope, arg_nodes);
                        }
                        _ => (), // Not a special form, fall through to regular call
                    }

                    // It is a regular function call or a builtin. Evaluate arguments.
                    let args = self.translate_arg_nodes(arg_nodes, scope)?;

                    match fn_name_str {
                        "+" => {
                            println!("rgs: {args:?}");
                            let res = self.builder.ins().iadd(args[0], args[1]);
                            return Ok(res);
                        }
                        _ => (), // Not a builtin, fall through to dynamic dispatch
                    }

                    let fn_sym = fn_name.into();
                    let func = self.load_symbol(scope, fn_sym, true)?;

                    let slot = self.builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size: 8 * args.len() as u32,
                        align_shift: 0,
                    });

                    for (i, val) in args.iter().enumerate() {
                        self.builder.ins().stack_store(*val, slot, i as i32 * 8);
                    }

                    let args_ptr = self.builder.ins().stack_addr(types::I64, slot, 0);
                    // let args_ptr = self.builder.ins().iconst(types::I64, args_ptr as i64);
                    let args_len = self.builder.ins().iconst(types::I64, args.len() as i64);
                    let env_ptr = scope.get_root() as *const Environment;
                    let env_ptr = self.builder.ins().iconst(types::I64, env_ptr as i64);

                    let res = self.call("apply", &[func, args_ptr, args_len, env_ptr])[0];
                    return Ok(res);
                }

                Err(CodegenError::InvalidSexpHead(head.clone()))
            }
            Node::Vector(_) => todo!(),
            Node::Integer(n) => {
                let val = RuntimeValue::tag(*n).0 as i64;
                println!("val: {val:?}, n: {n}");
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Node::Float(_) => todo!(),
            Node::Char(_) => {
                // let val = RuntimeValue::tag(*c);
                todo!()
            }
            Node::Str(string) => {
                let val = RuntimeValue::tag(LispString::from_str(string)).0 as i64;
                Ok(self.builder.ins().iconst(types::I64, val))
            }
            Node::Unquote => todo!(),
            Node::UnquoteSplice => todo!(),
            Node::Backquote => todo!(),
            Node::Nil => todo!(),
        }
    }

    pub fn translate_defun<'s>(
        &mut self,
        scope: &CompileScope<'s>,
        args: &[Node],
        body: &[Node],
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

        let result = codegen.translate_nodes(body, &new_scope)?;

        let func_id = codegen.func_id;
        let func = codegen.finalize(result);

        self.module.define_function(func_id, &mut ctx)?;
        let func_ptr = self.module.get_finalized_function(func_id);
        let closure: Function = func.try_into().unwrap();
        closure.set_func_ptr(func_ptr);

        self.module.clear_context(&mut ctx);
        Ok(closure)
    }

    pub fn translate_if<'s>(
        &mut self,
        scope: &CompileScope<'s>,
        cond: &Node,
        then: &Node,
        els: &[Node],
    ) -> CodegenResult<Value> {
        let cond_val = self.translate_node(cond, scope)?;
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
        let then_return = self.translate_node(then, scope)?;
        self.builder.ins().jump(merge_block, &[then_return.into()]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_return = self.translate_nodes(els, scope)?;
        self.builder.ins().jump(merge_block, &[else_return.into()]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        let phi = self.builder.block_params(merge_block)[0];
        Ok(phi)
    }

    pub fn translate_let<'s>(
        &mut self,
        parent_scope: &CompileScope<'s>,
        args: &[Node],
    ) -> CodegenResult<Value> {
        let bindings_node = args.get(0).ok_or(CodegenError::LetMissingBindingsAndBody)?;
        let body = &args[1..];

        let Node::Sexp(bindings) = bindings_node else {
            return Err(CodegenError::LetBindingsNotList);
        };

        let mut new_vars = HashMap::new();
        let mut binding_values = Vec::new();

        for binding in bindings {
            match binding {
                // Case 1: Just a symbol (var) - bind to NIL
                Node::Ident(ident) => {
                    let ident_str = ident.text();
                    let sym = Symbol::from_string(ident_str);
                    let var = self.builder.declare_var(types::I64);
                    new_vars.insert(sym, var);
                    binding_values.push(self.nil());
                }
                // Case 2: (var value) pair
                Node::Sexp(pair) => {
                    let (ident_node, value_expr) = match pair.as_slice() {
                        [id, val] => (id, val),
                        _ => return Err(CodegenError::LetInvalidBindingFormat),
                    };

                    let Node::Ident(ident) = ident_node else {
                        return Err(CodegenError::LetBindingNotSymbol);
                    };

                    let ident_str = ident.text();
                    let sym = Symbol::from_string(ident_str);
                    let var = self.builder.declare_var(types::I64);
                    new_vars.insert(sym, var);

                    // The values are evaluated in the *parent* scope.
                    let value = self.translate_node(value_expr, parent_scope)?;
                    binding_values.push(value);
                }
                _ => return Err(CodegenError::LetInvalidBindingFormat),
            }
        }

        let new_scope = FrameScope::new(new_vars, parent_scope, true, false).into();

        // Define the variables with their evaluated values
        if let CompileScope::Frame(frame) = &new_scope {
            // This re-iterates `bindings` which is a bit inefficient but safe and simple.
            for (binding, value) in bindings.iter().zip(binding_values) {
                let sym = match binding {
                    Node::Ident(ident) => {
                        let ident_str = ident.text();
                        Symbol::from_string(ident_str)
                    }
                    Node::Sexp(pair) => {
                        let Node::Ident(ident) = &pair[0] else {
                            unreachable!()
                        };
                        let ident_str = ident.text();
                        Symbol::from_string(ident_str)
                    }
                    _ => unreachable!(),
                };

                let var = frame.slots.get(sym).unwrap();
                self.builder.def_var(var, value);
            }
        } else {
            unreachable!();
        }

        // translate body
        let result = self.translate_nodes(body, &new_scope)?;

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
