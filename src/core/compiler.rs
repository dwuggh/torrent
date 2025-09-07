pub type DeclSubr = for<'a> fn(
    module: &'a mut cranelift_jit::JITModule,
) -> anyhow::Result<(String, cranelift_module::FuncId)>;
pub type DeclJITSym = for<'a> fn(&'a mut cranelift_jit::JITBuilder);

pub(crate) struct BuiltinFnPlugin {
    decl_subr: DeclSubr,
    decl_jit_sym: DeclJITSym,
    pub lisp_subr: bool,
}

inventory::collect!(BuiltinFnPlugin);

impl BuiltinFnPlugin {
    pub(crate) const fn new(
        decl_subr: DeclSubr,
        decl_jit_sym: DeclJITSym,
        lisp_subr: bool,
    ) -> Self {
        Self {
            decl_subr,
            decl_jit_sym,
            lisp_subr,
        }
    }
}

pub mod ast_to_ir;
mod codegen;
pub mod error;
pub mod ir;
pub mod jit;
pub mod scope;
