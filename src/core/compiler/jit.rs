use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;

use crate::core::compiler::codegen::Codegen;
use crate::core::compiler::{BuiltinFnPlugin, InternalFnPlugin};
use crate::core::env::Environment;
use crate::core::function::LispFunction;
use crate::core::parser::expr::Expr;
use crate::core::runtime::store_symbol_function;
use crate::core::symbol::Symbol;
use crate::core::tagged_ptr::TaggedObj;
use anyhow::Result;

pub struct JIT {
    data_desc: DataDescription,
    module: JITModule,
    builtin_funcs: HashMap<String, FuncId>,
}

impl JIT {
    pub fn new(env: &Environment) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "true").unwrap();
        // flag_builder.set("enable_safepoints", "true").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("opt_level", "speed").unwrap();
        // flag_builder.set("enable_verifier", "false").unwrap();
        flag_builder.set("enable_safepoints", "true").unwrap();
        flag_builder.set("preserve_frame_pointers", "true").unwrap();
        let flags = settings::Flags::new(flag_builder);
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(flags).unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // builder.symbol("apply", apply as *const u8);
        for func in inventory::iter::<BuiltinFnPlugin> {
            (func.decl_jit_sym)(&mut builder);
        }
        for func in inventory::iter::<InternalFnPlugin> {
            (func.decl_jit_sym)(&mut builder);
        }

        let mut module = JITModule::new(builder);
        let mut builtin_funcs = HashMap::new();

        // Load builtin functions (lisp subrs)
        for func in inventory::iter::<BuiltinFnPlugin> {
            let (name, id) = (func.decl_subr)(&mut module).unwrap();
            tracing::debug!("loading builtin function {name:?}...");
            let func = LispFunction::new_subr(id, &name, func.ptr(), func.signature);
            let symbol = Symbol::from(name);
            store_symbol_function(symbol, func.tag(), env);
        }

        // Load internal functions
        for func in inventory::iter::<InternalFnPlugin> {
            let (name, id) = (func.decl_subr)(&mut module).unwrap();
            tracing::debug!("loading internal function {name:?}...");
            builtin_funcs.insert(name, id);
        }
        Self {
            data_desc: DataDescription::new(),
            builtin_funcs,
            module,
        }
    }

    pub fn compile_expr(&mut self, expr: &Expr) -> Result<*const u8> {
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();
        let mut codegen = Codegen::new_empty(
            &mut self.module,
            &mut self.data_desc,
            &self.builtin_funcs,
            &mut fctx,
            &mut ctx,
        )?;
        let val = codegen.translate_expr(expr)?;

        let func_id = codegen.func_id;
        let (_, funcs) = codegen.finalize(val);

        self.module.define_function(func_id, &mut ctx)?;
        self.module.finalize_definitions()?;
        let f = self.module.get_finalized_function(func_id);
        self.module.clear_context(&mut ctx);
        tracing::info!("compile done: {funcs:?}");
        Ok(f)
    }
}
