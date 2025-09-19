use std::collections::HashMap;

use crate::core::error::{RuntimeError, RuntimeResult};
use crate::core::ident::Ident;
use cranelift_module::FuncId;
use proc_macros::Trace;
use rustc_hash::FxHashMap;

use crate::core::env::Environment;
use crate::{
    core::object::{LispType, Object},
    core::Tagged,
    gc::Gc,
};

#[derive(Debug, Clone, Trace)]
pub struct LispFunction(pub(crate) Gc<Function>);
impl_tagged_for_gc!(LispFunction, LispType::Function, Function);

#[derive(Debug, Clone, Trace)]
pub struct Function {
    #[no_trace]
    pub func_id: FuncId,

    #[no_trace]
    pub func_type: FunctionType,

    #[no_trace]
    pub func_ptr: Option<FuncPtr>,
    // #[no_trace]
    // signature: FunctionSignature,
}

#[derive(Debug, Clone, Trace)]
pub enum FunctionType {
    #[no_trace]
    Subr(SubrFn),
    Lambda(LambdaFn),
}

#[derive(Debug, Clone, Copy)]
pub struct SubrFn {
    name: Ident,
}

pub(crate) type FuncPtr =
    unsafe extern "C" fn(args_ptr: *const Object, argc: u64, env: *const Environment) -> Object;

pub unsafe fn cast_func_ptr(ptr: *const u8) -> FuncPtr {
    std::mem::transmute(ptr)
}

// TODO closures
#[derive(Debug, Clone, Trace)]
pub struct LambdaFn {
    // Captured environment as a GC’d map of Value -> Value (key is typically a Symbol tagged as Value).
    #[no_trace]
    pub captures: FxHashMap<Ident, Object>,
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

impl FunctionType {
    pub fn as_closure_mut(&mut self) -> Option<&mut LambdaFn> {
        if let FunctionType::Lambda(func) = self {
            Some(func)
        } else {
            None
        }
    }
}

impl LispFunction {
    pub fn new_closure(func_id: FuncId) -> Self {
        let closure = LambdaFn {
            captures: HashMap::new(),
        };
        let inner = Gc::new(Function {
            func_id,
            func_type: FunctionType::Lambda(closure),
            func_ptr: None,
        });
        Self(inner)
    }

    pub fn new_subr(func_id: FuncId, name: &str, func_ptr: *const u8) -> Self {
        let fptr = unsafe { cast_func_ptr(func_ptr) };
        let name = name.into();
        let inner = Gc::new(Function {
            func_id,
            func_type: FunctionType::Subr(SubrFn { name }),
            func_ptr: Some(fptr),
        });
        Self(inner)
    }

    pub fn func_id(&self) -> FuncId {
        self.0.get().func_id
    }

    pub fn get_func_type_mut(&self) -> &mut FunctionType {
        &mut self.0.get_mut().func_type
    }

    pub fn set_func_ptr(&self, func_ptr: *const u8) {
        unsafe { self.0.get_mut().func_ptr = Some(cast_func_ptr(func_ptr)) }
    }

    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        self.0.get().func_ptr
    }

    pub fn run(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        let func = self
            .0
            .get()
            .func_ptr
            .ok_or(RuntimeError::internal_error("no function pointer"))?;
        let argc = args.len() as u64;
        let result = unsafe { func(args.as_ptr(), argc, env) };
        Ok(result)
    }
}

impl Function {
    pub fn run(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        let func = self
            .func_ptr
            .ok_or(RuntimeError::internal_error("no function pointer"))?;
        let argc = args.len() as u64;
        let result = unsafe { func(args.as_ptr(), argc, env) };
        Ok(result)
    }

    pub fn get_func_type_mut(&mut self) -> &mut FunctionType {
        &mut self.func_type
    }
}
