use std::collections::HashMap;

use crate::core::ident::Ident;
use cranelift_module::FuncId;
use proc_macros::Trace;

use crate::core::env::Environment;
use crate::{
    core::value::{LispType, TaggedPtr, Value},
    gc::{Gc, GcInner},
};

#[derive(Debug, Clone, Copy)]
pub struct SubrFn {
    name: Ident,
}

pub(crate) type FuncPtr =
    unsafe extern "C" fn(args_ptr: *const Value, argc: u64, env: *const Environment) -> Value;

pub unsafe fn cast_func_ptr(ptr: *const u8) -> FuncPtr {
    std::mem::transmute(ptr)
}

// TODO closures
#[derive(Debug, Clone, Trace)]
pub struct LambdaFn {
    // Captured environment as a GC’d map of Value -> Value (key is typically a Symbol tagged as Value).
    #[no_trace]
    pub captures: HashMap<Ident, Value>,
}

#[derive(Debug, Clone, Trace)]
pub enum FunctionType {
    #[no_trace]
    Subr(SubrFn),
    Lambda(LambdaFn),
}

#[derive(Debug, Clone, Trace)]
pub struct FunctionInner {
    #[no_trace]
    pub func_id: FuncId,

    #[no_trace]
    pub func_type: FunctionType,

    #[no_trace]
    pub func_ptr: Option<FuncPtr>,

    // #[no_trace]
    // signature: FunctionSignature,
}

#[allow(unused)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionSignature {
    /// minial argument number required
    pub min_argc: u8,
    pub option_argc: u8,
    /// whether this function accepts variable args, like `&rest`
    pub variable_arg: bool,
}

#[derive(Debug, Clone, Trace)]
pub struct Function {
    pub(crate) inner: Gc<FunctionInner>,
}

impl FunctionType {
    pub fn as_closure_mut(&mut self) -> Option<&mut LambdaFn> {
        if let FunctionType::Lambda(func) = self {
            Some(func)
        } else {
            None
        }
    }
}

impl Function {
    pub fn new_closure(func_id: FuncId) -> Self {
        let closure = LambdaFn {
            captures: HashMap::new(),
        };
        Self {
            inner: Gc::new(FunctionInner {
                func_id,
                func_type: FunctionType::Lambda(closure),
                func_ptr: None,
            }),
        }
    }

    pub fn new_subr(func_id: FuncId, name: &str, func_ptr: *const u8) -> Self {
        let fptr = unsafe { cast_func_ptr(func_ptr) };
        let name = name.into();
        Self {
            inner: Gc::new(FunctionInner {
                func_id,
                func_type: FunctionType::Subr(SubrFn { name }),
                func_ptr: Some(fptr),
            }),
        }
    }

    pub fn func_id(&self) -> FuncId {
        self.inner.get().func_id
    }

    pub fn get_func_type_mut(&self) -> &mut FunctionType {
        &mut self.inner.get_mut().func_type
    }

    pub fn set_func_ptr(&self, func_ptr: *const u8) {
        unsafe { self.inner.get_mut().func_ptr = Some(cast_func_ptr(func_ptr)) }
    }

    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        self.inner.get().func_ptr
    }

    pub fn run(&self, args: &[Value], env: &Environment) -> anyhow::Result<Value> {
        let func = self
            .inner
            .get()
            .func_ptr
            .ok_or(anyhow::anyhow!("no pointer"))?;
        let argc = args.len() as u64;
        let result = unsafe { func(args.as_ptr(), argc, env) };
        Ok(result)
    }
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
