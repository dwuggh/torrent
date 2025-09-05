use std::collections::HashMap;

use anyhow::bail;
use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;

use crate::ast::Node;
use crate::core::compiler::BuiltinFnPlugin;
use crate::core::env::{Environment, ParamSlots};
use crate::core::string::LispString;
use crate::core::symbol::Symbol;
use crate::core::value::Value as RuntimeValue;
use anyhow::Result;

pub struct JIT {
    data_desc: DataDescription,
    module: JITModule,
    builtin_funcs: HashMap<String, FuncId>
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // builder.symbol("apply", apply as *const u8);
        for func in inventory::iter::<BuiltinFnPlugin> {
            (func.decl_jit_sym)(&mut builder);
        }

        let mut module = JITModule::new(builder);
        let mut builtin_funcs = HashMap::new();
        for func in inventory::iter::<BuiltinFnPlugin> {
            let (name, id) = (func.decl_subr)(&mut module).unwrap();
            builtin_funcs.insert(name, id);
        }
        Self {
            data_desc: DataDescription::new(),
            builtin_funcs,
            module,
        }
    }
}

impl JIT {
    pub fn compile_node(&mut self, node: &Node, env: CompileScope) -> Result<*const u8> {
        let mut sig = self.module.make_signature();
        // sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        let func_id = self.module.declare_anonymous_function(&sig)?;
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();
        ctx.func.signature = sig;
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let res = self.translate_node(node, &mut builder, env)?;
        println!("result: {res:?}");
        builder.ins().return_(&[res]);
        builder.finalize();
        self.module.define_function(func_id, &mut ctx)?;

        self.module.finalize_definitions()?;
        let f = self.module.get_finalized_function(func_id);
        self.module.clear_context(&mut ctx);
        return Ok(f);
    }

    fn translate_node(
        &mut self,
        node: &Node,
        builder: &mut FunctionBuilder,
        env: CompileScope,
    ) -> anyhow::Result<Value> {
        match node {
            Node::Ident(ident) => {
                let symbol = Symbol::from_string(ident);
                env.load_symbol(symbol, false, builder)
                    .ok_or(anyhow::anyhow!("symbol not found"))
            }
            Node::Sexp(nodes) => {
                if nodes.is_empty() {
                    return Ok(builder.ins().iconst(types::I64, 0));
                }

                let head = &nodes[0];
                let args = &nodes[1..];

                let args = args
                    .iter()
                    .map(|n| self.translate_node(n, builder, env))
                    .fold(Ok(Vec::new()), |args, r| {
                        args.and_then(|mut vec| {
                            r.map(|val| {
                                vec.push(val);
                                vec
                            })
                        })
                    })?;

                // self.module.declare_function(name, linkage, signature);
                match head {
                    Node::Ident(fn_name) => {
                        match fn_name.as_str() {
                            "+" => {
                                println!("rgs: {args:?}");
                                let res = builder.ins().iadd(args[0], args[1]);
                                return Ok(res)
                            }
                            _ => (),
                        }
                        let fn_sym = Symbol::from_string(fn_name);
                        let func = env.load_symbol(fn_sym, true, builder).unwrap();
                        // let LispValue::Function(func) = value.untag() else {
                        //     bail!("not a function in func cell");
                        // };
                        // let func_id = def_apply(&mut self.module);
                        let func_id = *self.builtin_funcs.get("apply").unwrap();

                        let apply = self.module.declare_func_in_func(func_id, builder.func);

                        // collect evaled args: Iter<Item=Result<Value>> -> Result<Vec<Value>>
                        let argcnt = args.len();
                        let args_ptr = args.as_ptr();
                        let args_ptr = builder.ins().iconst(types::I64, args_ptr as i64);
                        let args_len = builder.ins().iconst(types::I64, args.len() as i64);
                        let env_ptr = env.get_root();
                        let env_ptr = builder.ins().iconst(types::I64, env_ptr as i64);
                        let result = builder
                            .ins()
                            .call(apply, &[func, args_ptr, args_len, env_ptr]);
                        let res = builder.inst_results(result)[0];
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
                Ok(builder.ins().iconst(types::I64, val))
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
                Ok(builder.ins().iconst(types::I64, val))
            }
            Node::Unquote => todo!(),
            Node::UnquoteSplice => todo!(),
            Node::Backquote => todo!(),
            Node::Nil => todo!(),
        }
    }

    fn translate_defun(
        &mut self,
        name: &str,
        args: &[Node],
        body: &[Node],
        env: CompileScope,
    ) -> anyhow::Result<()> {
        let mut sig = self.module.make_signature();
        for _ in 0..args.len() {
            sig.params.push(AbiParam::new(types::I64));
        }
        // NOTE env arg
            sig.params.push(AbiParam::new(types::I64));
        // self.module.declare_data("", linkage, writable, tls);
        sig.returns.push(AbiParam::new(types::I64));
        let func_id =
            self.module
                .declare_function(name, cranelift_module::Linkage::Export, &sig)?;
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();
        ctx.func.signature = sig;
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // builder.block_params(entry_block);

        let mut variables = HashMap::new();
        for (i, arg) in args.iter().enumerate() {
            let Node::Ident(arg) = arg else {
                bail!("wrong function arg format")
            };
            let sym = Symbol::from_string(arg);
            let val = builder.block_params(entry_block)[i];
            let var = builder.declare_var(types::I64);
            variables.insert(sym, var);
            builder.def_var(var, val);
        }

        // create a Function Envrionment
        let scope = FrameScope::new(variables, env);
        let func_env = (&scope).into();

        let res = body
            .iter()
            .map(|n| self.translate_node(n, &mut builder, func_env))
            .reduce(|a, b| a.and(b))
            .unwrap()?;

        builder.ins().return_(&[res]);
        builder.finalize();
        // translator.builder.ins().return_(&[result]);
        // translator.builder.finalize();

        self.module.define_function(func_id, &mut ctx)?;
        self.module.clear_context(&mut ctx);

        todo!()
    }
}
#[derive(Clone, Copy)]
pub struct GlobalScope {
    env: *mut Environment,
}

impl GlobalScope {
    pub fn new(env: *mut Environment) -> Self {
        Self { env }
    }
}

#[derive(Clone)]
pub struct FrameScope<'a> {
    slots: ParamSlots,
    parent: CompileScope<'a>,
}

#[derive(Clone, Copy)]
pub enum CompileScope<'a> {
    Global(&'a GlobalScope),
    Frame(&'a FrameScope<'a>),
}

impl<'a> FrameScope<'a> {
    pub fn new(vars: HashMap<Symbol, Variable>, parent: CompileScope<'a>) -> Self {
        FrameScope {
            slots: ParamSlots::new(vars),
            parent,
        }
    }
}

impl<'a> From<&'a FrameScope<'a>> for CompileScope<'a> {
    fn from(value: &'a FrameScope<'a>) -> Self {
        CompileScope::Frame(value)
    }
}

impl CompileScope<'_> {
    fn get_root(self) -> *mut Environment {
        match self {
            CompileScope::Global(root) => root.env,
            CompileScope::Frame(frame) => frame.parent.get_root(),
        }
    }

    fn load_symbol(
        self,
        symbol: Symbol,
        load_function_cell: bool,
        builder: &mut FunctionBuilder,
    ) -> Option<Value> {
        self.load_symbol_inner(symbol, load_function_cell, builder, true)
    }

    fn load_symbol_inner(
        self,
        symbol: Symbol,
        load_function_cell: bool,
        builder: &mut FunctionBuilder,
        same_func_scope: bool,
    ) -> Option<Value> {
        match self {
            CompileScope::Global(_) => {
                let val = Environment::default().load_symbol(symbol, load_function_cell)?;
                Some(builder.ins().iconst(types::I64, val.0 as i64))
            }
            CompileScope::Frame(frame) => same_func_scope
                .then(|| {
                    let var = frame.slots.get(symbol)?;
                    Some(builder.use_var(var))
                })
                .flatten()
                .or(frame.parent.load_symbol_inner(
                    symbol,
                    load_function_cell,
                    builder,
                    false,
                )),
        }
    }
}
