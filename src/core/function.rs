use cranelift::prelude::types;
use cranelift::prelude::AbiParam;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::FuncId;
use cranelift_module::Module;
use proc_macros::Trace;

use crate::{
    core::{
        env::Env,
        value::{LispType, TaggedPtr, Value},
    },
    gc::{Gc, GcInner, Trace},
};

pub(crate) type BuiltInFn =
    for<'a> fn(args: *const i64, argcnt: u64, env: *mut Env) -> anyhow::Result<Value>;

#[derive(Debug, Clone, Copy)]
pub struct SubrFn {
    pub func: BuiltInFn,
    argcnt: usize,
}

// TODO closures
#[derive(Debug, Clone)]
pub struct LambdaFn {}

#[derive(Debug, Clone)]
enum FunctionType {
    Subr(SubrFn),
    Lambda(LambdaFn),
}

#[derive(Debug, Clone, Trace)]
struct FunctionInner {
    #[no_trace]
    pub func_id: FuncId,
    #[no_trace]
    pub func_type: FunctionType,
}

#[derive(Debug, Clone)]
pub struct Function {
    inner: Gc<FunctionInner>,
}

impl Function {
    pub fn func_id(&self) -> FuncId {
        self.inner.get().func_id
    }

    pub fn declare_subr(subr_fn: SubrFn, module: &mut JITModule, name: &str) -> anyhow::Result<Self> {
        let mut sig = module.make_signature();
        for i in 0..subr_fn.argcnt {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        let func_id = module.declare_function(name, cranelift_module::Linkage::Import, &sig)?;

        Ok(Self {
            inner: Gc::new(FunctionInner {
                func_id,
                func_type: FunctionType::Subr(subr_fn),
            }),
        })
        // module.define_function(, ctx)
    }

    pub fn subr_import_to_jit(subr_fn: SubrFn, builder: &mut JITBuilder, name: &str) {
        builder.symbol(name, subr_fn.func as *const u8);
    }
    // pub fn eval(&self, args: &[Value], env: &mut Env) -> anyhow::Result<Value> {
    //     let func = self.inner.get();
    //     (func.func)(args, env)
    // }
}

unsafe impl Trace for Function {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {}
}

impl TaggedPtr for Function {
    const TAG: super::value::LispType = LispType::Function;

    unsafe fn cast(val: u64) -> Self {
        Function {
            inner: Gc::from_raw(val as *mut GcInner<FunctionInner>),
        }
    }

    unsafe fn get_untagged_data(self) -> u64 {
        Gc::into_raw(self.inner) as u64
    }
}
