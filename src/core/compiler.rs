use crate::core::function::FunctionSignature;

pub type DeclSubr = for<'a> fn(
    module: &'a mut cranelift_jit::JITModule,
) -> anyhow::Result<(String, cranelift_module::FuncId)>;
pub type DeclJITSym = for<'a> fn(&'a mut cranelift_jit::JITBuilder);

pub struct FPtr(*const u8);

unsafe impl Send for FPtr {}
unsafe impl Sync for FPtr {}

pub(crate) struct BuiltinFnPlugin {
    decl_subr: DeclSubr,
    decl_jit_sym: DeclJITSym,
    pub lisp_subr: bool,
    pub ptr: FPtr,
    // pub signature: FunctionSignature,
}

inventory::collect!(BuiltinFnPlugin);

impl BuiltinFnPlugin {
    pub(crate) const fn new(
        decl_subr: DeclSubr,
        decl_jit_sym: DeclJITSym,
        lisp_subr: bool,
        ptr: *const u8,
    ) -> Self {
        Self {
            decl_subr,
            decl_jit_sym,
            lisp_subr,
            ptr: FPtr(ptr),
        }
    }
    pub fn ptr(&self) -> *const u8 {
        self.ptr.0
    }
}

pub mod ast_to_ir;
mod codegen;
pub mod error;
pub mod ir;
pub mod jit;
pub mod scope;
pub mod node_parser;
