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
use crate::core::compiler::scope::CompileScope;
use crate::core::compiler::scope::FrameScope;
use crate::core::compiler::scope::Val;
use crate::core::env::Environment;
use crate::core::function::Function;
use crate::core::string::LispString;
use crate::core::symbol::Symbol;
use crate::core::value::TaggedPtr;
use crate::core::value::NIL;
use crate::Value as RuntimeValue;

pub struct Codegen<'a, 's> {
    module: &'a mut JITModule,
    builder: FunctionBuilder<'a>,
    scope: CompileScope<'s>,
    captures: HashSet<Symbol>,
    closure: Value,
    func_id: FuncId,
    builtin_funcs: &'a HashMap<String, FuncId>,
}

impl<'a, 's> Codegen<'a, 's> {
    pub fn new(
        module: &'a mut JITModule,
        builtin_funcs: &'a HashMap<String, FuncId>,
        fctx: &'a mut FunctionBuilderContext,
        ctx: &'a mut Context,
        args: &[Node],
        prev_scope: &'s CompileScope<'s>,
    ) -> Result<Self> {
        // make signature

        let sig = &mut ctx.func.signature;
        for _ in 0..args.len() {
            sig.params.push(AbiParam::new(types::I64));
        }
        // NOTE env arg
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        let func_id = module.declare_anonymous_function(
            &ctx.func.signature,
        )?;
        let mut builder = FunctionBuilder::new(&mut ctx.func, fctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // builder.block_params(entry_block);

        let mut variables = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let Node::Ident(arg) = arg else {
                anyhow::bail!("wrong function arg format")
            };
            let sym = Symbol::from_string(arg);
            let val = builder.block_params(entry_block)[i];
            let var = builder.declare_var(types::I64);
            variables.insert(sym, var);
            builder.def_var(var, val);
        }

        let captures = HashSet::new();
        let closure = Function::new_closure(0 as *const u8, func_id).tag();
        let closure = load_value(&mut builder, closure);

        let scope = FrameScope::new(variables, prev_scope, false).into();

        Ok(Self {
            module,
            builtin_funcs,
            builder,
            captures,
            closure,
            func_id,
            scope,
        })
    }

    fn nil(&mut self) -> Value {
        self.builder.ins().iconst(types::I64, NIL)
    }

    pub fn translate_nodes(&mut self, nodes: &[Node]) -> Result<Value> {
        nodes
            .iter()
            .map(|n| self.translate_node(n))
            .reduce(|a, b| a.and(b))
            .unwrap_or(Ok(self.nil()))
    }

    pub fn finalize(mut self, val: Value) {
        self.builder.ins().return_(&[val]);
        self.builder.finalize();
    }

    pub fn load_symbol(&mut self, symbol: Symbol) -> Result<Value> {
        self.scope
            .load_symbol(symbol, false, &mut self.builder)
            .ok_or(anyhow::anyhow!("symbol not found"))
            .map(|val| match val {
                Val::Value(value) => value,
                Val::Symbol(symbol) => {
                    self.captures.insert(symbol);
                    // self.builder.ins().call(FN, args);
                    let func_id = *self.builtin_funcs.get("--load-captured").unwrap();
                    let sym = load_value(&mut self.builder, symbol.tag());

                    let load = self.module.declare_func_in_func(func_id, self.builder.func);
                    let inst = self.builder.ins().call(load, &[sym, self.closure]);
                    self.builder.inst_results(inst)[0]
                }
            })
    }

    pub fn translate_node(&mut self, node: &Node) -> anyhow::Result<Value> {
        match node {
            Node::Ident(ident) => {
                let symbol = Symbol::from_string(ident);
                let val = self
                    .load_symbol(symbol)?;
                Ok(val)
            }
            Node::Sexp(nodes) => {
                if nodes.is_empty() {
                    return Ok(self.builder.ins().iconst(types::I64, 0));
                }

                let head = &nodes[0];
                let args = &nodes[1..];

                let args = args.iter().map(|n| self.translate_node(n)).fold(
                    Ok(Vec::new()),
                    |args, r| {
                        args.and_then(|mut vec| {
                            r.map(|val| {
                                vec.push(val);
                                vec
                            })
                        })
                    },
                )?;

                // self.module.declare_function(name, linkage, signature);
                match head {
                    Node::Ident(fn_name) => {
                        match fn_name.as_str() {
                            "+" => {
                                println!("rgs: {args:?}");
                                let res = self.builder.ins().iadd(args[0], args[1]);
                                return Ok(res);
                            }
                            _ => (),
                        }
                        let fn_sym = Symbol::from_string(fn_name);
                        let func = self
                            .scope
                            .load_symbol(fn_sym, true, &mut self.builder)
                            .unwrap();
                        // let LispValue::Function(func) = value.untag() else {
                        //     bail!("not a function in func cell");
                        // };
                        // let func_id = def_apply(&mut self.module);
                        let func_id = *self.builtin_funcs.get("apply").unwrap();

                        let apply = self.module.declare_func_in_func(func_id, self.builder.func);

                        // collect evaled args: Iter<Item=Result<Value>> -> Result<Vec<Value>>
                        let argcnt = args.len();
                        let args_ptr = args.as_ptr();
                        let args_ptr = self.builder.ins().iconst(types::I64, args_ptr as i64);
                        let args_len = self.builder.ins().iconst(types::I64, args.len() as i64);
                        let env_ptr = self.scope.get_root() as *const Environment;
                        let env_ptr = self.builder.ins().iconst(types::I64, env_ptr as i64);
                        let result = self
                            .builder
                            .ins()
                            .call(apply, &[func, args_ptr, args_len, env_ptr]);
                        let res = self.builder.inst_results(result)[0];
                        // let a = builder.ins().return_(&[res]);
                        Ok(res)
                    }
                    Node::Unquote => todo!(),
                    Node::UnquoteSplice => todo!(),
                    Node::Backquote => todo!(),
                    _ => {
                        anyhow::bail!("invalid function {node:?}");
                    }
                }
            }
            Node::Vector(nodes) => todo!(),
            Node::Integer(n) => {
                let val = RuntimeValue::tag(*n).0 as i64;
                println!("val: {val:?}, n: {n}");
                Ok(self.builder.ins().iconst(types::I64, val))
                // Ok(builder.ins().iconst(types::I64, *n as i64))
                // Variable
            }

            Node::Float(_) => todo!(),
            Node::Char(c) => {
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

    pub fn translate_defun(
        &mut self,
        name: &str,
        args: &[Node],
        body: &[Node],
    ) -> anyhow::Result<Function> {
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();

        let mut codegen = Codegen::new(
            self.module,
            self.builtin_funcs,
            &mut fctx,
            &mut ctx,
            args,
            &self.scope,
        )?;

        let result = codegen.translate_nodes(body)?;
        codegen.finalize(result);

        // translator.builder.ins().return_(&[result]);
        // translator.builder.finalize();

        let func_id = self.module.declare_function(
            name,
            cranelift_module::Linkage::Export,
            &ctx.func.signature,
        )?;

        self.module.define_function(func_id, &mut ctx)?;
        self.module.clear_context(&mut ctx);
        let func_ptr = self.module.get_finalized_function(func_id);
        let closure = Function::new_closure(func_ptr, func_id);
        Ok(closure)
    }

    pub fn translate_let(&mut self, body: &[Node]) -> Result<()> {
        todo!()
    }
}

fn load_value(builder: &mut FunctionBuilder, val: RuntimeValue) -> Value {
    builder.ins().iconst(types::I64, val.0 as i64)
}