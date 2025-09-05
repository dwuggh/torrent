use std::collections::HashMap;
use std::sync::Arc;

use anyhow::bail;
use cranelift::codegen;
use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;

use crate::ast::Node;
use crate::core::env::Env;
use crate::core::env::Environment;
use crate::core::env::General;
use crate::core::env::LexicalScope;
use crate::core::env::ParamsMap;
use crate::core::env::ParamsScope;
use crate::core::symbol::Symbol;
use crate::core::value::LispValue;
use crate::core::value::Value as RuntimeValue;
use crate::gc::Gc;
use anyhow::Result;

pub struct JIT {
    data_desc: DataDescription,
    module: JITModule,
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

        builder.symbol("apply", apply as *const u8);

        let module = JITModule::new(builder);
        Self {
            data_desc: DataDescription::new(),
            module,
        }
    }
}

impl JIT {
    pub fn compile_node(&mut self, node: &Node, env: ScopeCtx) -> Result<*const u8> {
        let mut sig = self.module.make_signature();
        // sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        // let func_id = self.module.declare_anonymous_function(&sig)?;
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
        let func_id = self.module.declare_function("test", cranelift_module::Linkage::Export, &ctx.func.signature)?;
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
        env: ScopeCtx,
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
                        let func_id = def_apply(&mut self.module);

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
                let val = RuntimeValue::tag(Arc::from(string.clone())).0 as i64;
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
        env: ScopeCtx,
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
        let mut scope = ParamsCtx::new(variables, env);
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

pub type InstallSubr = for<'a> fn(module: &'a mut cranelift_jit::JITModule) -> FuncId;

pub(crate) struct BuiltinFnPlugin {
    decl_subr: InstallSubr,
    decl_jit_sym: for<'a> fn(&'a mut cranelift_jit::JITBuilder),
}

impl BuiltinFnPlugin {
    pub(crate) const fn new(
        decl_subr: InstallSubr,
        decl_jit_sym: for<'a> fn(&'a mut cranelift_jit::JITBuilder),
    ) -> Self {
        Self {
            decl_subr,
            decl_jit_sym,
        }
    }
}

#[derive(Clone)]
pub struct ParamsCtx<'a> {
    scope: Arc<ParamsScope>,
    parent: ScopeCtx<'a>,
}

#[derive(Clone)]
pub struct RootCtx {
    env: Gc<General>,
}

impl RootCtx {
    pub fn new(env: Gc<General>) -> Self {
        Self { env }
    }
}

#[derive(Clone)]
pub struct LexicalCtx<'a> {
    env: Gc<LexicalScope>,
    parent: &'a ScopeCtx<'a>,
}

// we don't want to trigger GC when compiling
#[derive(Clone, Copy)]
pub enum ScopeCtx<'a> {
    Root(&'a RootCtx),
    Lexical(&'a LexicalCtx<'a>),
    Function(&'a ParamsCtx<'a>),
}

impl<'a> ParamsCtx<'a> {
    pub fn new(vars: ParamsMap, parent: ScopeCtx<'a>) -> Self {
        ParamsCtx {
            scope: Arc::new(ParamsScope::new(vars)),
            parent,
        }
    }
}

impl<'a> From<&'a ParamsCtx<'a>> for ScopeCtx<'a> {
    fn from(value: &'a ParamsCtx<'a>) -> Self {
        ScopeCtx::Function(value)
    }
}

impl ScopeCtx<'_> {
    fn get_root(self) -> *mut Environment {
        match self {
            ScopeCtx::Root(root_ctx) => {
                let mut env = Environment(root_ctx.env.clone());
                &mut env
            }
            ScopeCtx::Lexical(lexical_ctx) => lexical_ctx.parent.get_root(),
            ScopeCtx::Function(params_ctx) => params_ctx.parent.get_root(),
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
            ScopeCtx::Root(root_ctx) => {
                let val = root_ctx.env.get().load_symbol(symbol, load_function_cell)?;
                let value = builder.ins().iconst(types::I64, val.0 as i64);
                Some(value)
            }
            ScopeCtx::Lexical(lexical_ctx) => lexical_ctx
                .env
                .get()
                .vars
                .get(&symbol)
                .and_then(|cell| {
                    let data = cell.get().data();
                    let val = if load_function_cell {
                        data.func
                    } else {
                        data.value
                    }?;
                    Some(builder.ins().iconst(types::I64, val.0 as i64))
                })
                .or(lexical_ctx.parent.load_symbol_inner(
                    symbol,
                    load_function_cell,
                    builder,
                    same_func_scope,
                )),
            ScopeCtx::Function(params_ctx) => same_func_scope
                .then(|| {
                    let var = params_ctx.scope.load_params(symbol)?;
                    Some(builder.use_var(var))
                })
                .flatten()
                .or(params_ctx.parent.load_symbol_inner(
                    symbol,
                    load_function_cell,
                    builder,
                    false,
                )),
        }
    }
}

unsafe extern "C" fn apply(func: i64, args: *const i64, argcnt: i64, env: *mut Environment) -> i64 {
    let args: Vec<_> = (0..argcnt)
        .map(|i| RuntimeValue::from_raw_inc_rc(args.add(i as usize).read() as u64))
        .collect();
    let func = RuntimeValue(func as u64);
    match func.untag() {
        LispValue::Function(func) => {
            let val = func.run(&args, env.as_mut().unwrap()).unwrap();
            val.0 as i64
        }
        _ => panic!(),
    }
}

fn def_apply<T: Module>(module: &mut T) -> FuncId {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    let func = module
        .declare_function("apply", cranelift_module::Linkage::Import, &sig)
        .unwrap();

    // module.define_function(func, ctx);
    return func;

    // builder.symbol("apply", apply as *const u8);
}
