use std::collections::HashMap;

use crate::core::ident::Ident;
use crate::core::map::Map;
use cranelift_module::FuncId;
use proc_macros::Trace;

use crate::core::env::Environment;
use crate::core::symbol::Symbol;
use crate::core::symbol::SymbolCell;
use crate::{
    core::value::{LispType, TaggedPtr, Value},
    gc::{Gc, GcInner, Trace},
};

pub(crate) type BuiltInFn =
    for<'a> fn(args: *const Value, env: *mut Environment) -> anyhow::Result<Value>;

#[derive(Debug, Clone, Copy)]
pub struct SubrFn {
    pub func: BuiltInFn,
}

pub(crate) type Closure =
    for<'a> fn(args: *const Value, env: *mut Environment) -> anyhow::Result<Value>;

pub unsafe fn cast_func_ptr(ptr: *const u8) -> Closure {
    std::mem::transmute(ptr)
}

// TODO closures
#[derive(Debug, Clone, Trace)]
pub struct LambdaFn {
    // Captured environment as a GC’d map of Value -> Value (key is typically a Symbol tagged as Value).
    #[no_trace]
    pub captures: HashMap<Ident, Value>,
    #[no_trace]
    pub func: BuiltInFn,
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
    pub sig: FunctionSignature,

    #[no_trace]
    pub func_type: FunctionType,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionSignature {
    pub func_id: FuncId,
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
    pub fn new_closure(func: *const u8, func_id: FuncId) -> Self {
        unsafe {
            let func = cast_func_ptr(func);
            let closure = LambdaFn {
                captures: HashMap::new(),
                func,
            };
            Self {
                inner: Gc::new(FunctionInner {
                    func_id,
                    func_type: FunctionType::Lambda(closure),
                }),
            }
        }
    }

    pub fn func_id(&self) -> FuncId {
        self.inner.get().func_id
    }

    pub fn get_func_type_mut(&self) -> &mut FunctionType {
        &mut self.inner.get_mut().func_type
    }

    pub fn set_func_ptr(&self, func_ptr: *const u8) {
        match &mut self.inner.get_mut().func_type {
            FunctionType::Subr(subr_fn) => unsafe {
                subr_fn.func = cast_func_ptr(func_ptr);
            },
            FunctionType::Lambda(lambda_fn) => unsafe {
                lambda_fn.func = cast_func_ptr(func_ptr);
            },
        }
    }

    pub fn run(&self, args: &[Value], env: &mut Environment) -> anyhow::Result<Value> {
        match &self.inner.get().func_type {
            FunctionType::Subr(subr_fn) => (subr_fn.func)(args.as_ptr(), env),
            FunctionType::Lambda(lambda_fn) => (lambda_fn.func)(args.as_ptr(), env),
        }
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
