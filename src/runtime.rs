use std::collections::HashMap;

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
use crate::core::symbol::Symbol;
use crate::core::value::LispValue;
use crate::core::value::Value as RuntimeValue;
use anyhow::Result;

pub struct JIT {
    fctx: FunctionBuilderContext,
    ctx: codegen::Context,
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
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            fctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_desc: DataDescription::new(),
            module,
        }
    }
}

impl JIT {
    fn translate_node(&mut self, node: &Node) -> anyhow::Result<()> {
        todo!()
    }

    fn translate_defun(&mut self, name: &str, args: &[Node], body: &[Node], env: &mut Env) -> anyhow::Result<()> {
        let mut sig = self.module.make_signature();
        for _ in 0..args.len() {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        let func_id = self.module.declare_function(name, cranelift_module::Linkage::Export, &sig)?;        
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        builder.block_params(entry_block);

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
        // TODO create a Function Envrionment

        let mut translator = Translator {
            builder,
            module: &mut self.module,
        };

        let result = translator.translate_body(body, env)?;
        translator.builder.ins().return_(&[result]);
        translator.builder.finalize();

        self.module.define_function(func_id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);

        todo!()
    }

}

struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
}

impl<'a> Translator<'a> {

    fn translate_body(&mut self, body: &[Node], env: &mut Env) -> Result<Value> {
        todo!()
    }


    fn translate_node(&mut self, node: &Node, env: &mut Env) -> Result<Value> {
        match node {
            Node::Ident(ident) => {
                let symbol = Symbol::from_string(ident);
                let value = env.load_symbol(symbol, false);
                match value {
                    Some(val) => {
                        let result = self.builder.ins().iconst(types::I64, val.0 as i64);
                        return Ok(result);
                    }
                    None => anyhow::bail!("cannot find value for {ident}"),
                }
                // value.map()
            }
            Node::Sexp(nodes) => {
                if nodes.is_empty() {
                    return Ok(self.builder.ins().iconst(types::I64, 0));
                }

                let head = &nodes[0];
                let args = &nodes[1..];

                // self.module.declare_function(name, linkage, signature);
                match head {
                    Node::Ident(fn_name) => {
                        let fn_sym = Symbol::from_string(fn_name);
                        let value = env.load_symbol(fn_sym, true).unwrap();
                        let LispValue::Function(func) = value.untag() else {
                            bail!("not a function in func cell");
                        };

                        let func = self.module.declare_func_in_func(func.func_id(), self.builder.func);
                        // collect evaled args: Iter<Item=Result<Value>> -> Result<Vec<Value>>


                        let args = args.iter().map(|n| self.translate_node(n, env)).fold(
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
                        let argcnt = args.len();
                        self.builder.ins().call(func, &args);
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
                self.builder.ins().iconst(types::I64, val);
                // Variable
            }

            Node::Float(_) => todo!(),
            Node::Char(_) => todo!(),
            Node::Str(_) => todo!(),
            Node::Unquote => todo!(),
            Node::UnquoteSplice => todo!(),
            Node::Backquote => todo!(),
            Node::Nil => todo!(),
        }
        todo!()
    }

    fn translate_function(&mut self, head: &Node, args: &[Node], env: &mut Env) -> Result<Value> {
        // let mut sig = self.module.make_signature();
        // self.module.declare_function(name, cranelift_module::Linkage::Export, &sig);
        todo!()
    }
    fn translate_macro(&mut self, head: &Node, args: &[Node], env: &mut Env) -> Result<Value> {
        todo!()
    }

    fn translate_defun(&mut self, func_name: &str, args: &[Symbol], body: &Node, env: &mut Env) -> Result<Value> {
        let mut sig = self.module.make_signature();
        for _ in 0..args.len() {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        let func_id = self.module.declare_function(func_name, cranelift_module::Linkage::Export, &sig)?;        

        let builder = &mut self.builder;
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        builder.block_params(entry_block);
        let val = self.translate_node(body, env)?;
        builder.ins().return_(&[val]);
        builder.finalize();
        todo!()
    }
}

pub type InstallSubr = for<'a> fn(module: &'a mut cranelift_jit::JITModule) -> FuncId;

pub(crate) struct RuntimeFn {
    install_subr: InstallSubr,
    install_symbol: for<'a> fn(&'a mut cranelift_jit::JITBuilder),
}

impl RuntimeFn {
    pub(crate) const fn new(
        install_subr: InstallSubr,
        install_symbol: for<'a> fn(&'a mut cranelift_jit::JITBuilder),
    ) -> Self {
        Self {
            install_subr,
            install_symbol,
        }
    }
}


