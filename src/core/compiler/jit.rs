use std::collections::HashMap;

use anyhow::bail;
use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::DataDescription;
use cranelift_module::FuncId;
use cranelift_module::Module;

use crate::ast::Node;
use crate::core::compiler::codegen::Codegen;
use crate::core::compiler::BuiltinFnPlugin;
use super::scope::CompileScope;
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
    pub fn compile_node(&mut self, node: &Node, scope: &CompileScope) -> Result<*const u8> {
        let mut fctx = FunctionBuilderContext::new();
        let mut ctx = self.module.make_context();
        let mut codegen = Codegen::new(&mut self.module, &self.builtin_funcs, &mut fctx, &mut ctx, &[], scope)?;
        let val = codegen.translate_node(node)?;
        codegen.finalize(val);

        let func_id = self.module.declare_anonymous_function(&ctx.func.signature)?;
        self.module.define_function(func_id, &mut ctx)?;
        self.module.finalize_definitions()?;
        let f = self.module.get_finalized_function(func_id);
        self.module.clear_context(&mut ctx);
        return Ok(f);
    }

}

