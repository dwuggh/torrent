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
    pub ptr: FPtr,
    pub signature: crate::core::function::FunctionSignature,
}

pub(crate) struct InternalFnPlugin {
    decl_subr: DeclSubr,
    decl_jit_sym: DeclJITSym,
    pub ptr: FPtr,
}

inventory::collect!(BuiltinFnPlugin);
inventory::collect!(InternalFnPlugin);

impl BuiltinFnPlugin {
    pub(crate) const fn new(
        decl_subr: DeclSubr,
        decl_jit_sym: DeclJITSym,
        ptr: *const u8,
        signature: crate::core::function::FunctionSignature,
    ) -> Self {
        Self {
            decl_subr,
            decl_jit_sym,
            ptr: FPtr(ptr),
            signature,
        }
    }
    pub fn ptr(&self) -> *const u8 {
        self.ptr.0
    }
}

impl InternalFnPlugin {
    pub(crate) const fn new(decl_subr: DeclSubr, decl_jit_sym: DeclJITSym, ptr: *const u8) -> Self {
        Self {
            decl_subr,
            decl_jit_sym,
            ptr: FPtr(ptr),
        }
    }
    pub fn ptr(&self) -> *const u8 {
        self.ptr.0
    }
}

mod codegen;
pub mod error;
pub mod jit;
pub mod macro_item;
// pub mod optimization;
// pub mod scope;
