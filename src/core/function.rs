use crate::core::error::{RuntimeError, RuntimeResult};
use crate::core::ident::Ident;
use crate::core::parser::expr::Args;
use cranelift_module::FuncId;
use proc_macros::Trace;
use rustc_hash::FxHashMap;

use crate::core::env::Environment;
use crate::{
    core::object::{LispType, Object},
    core::Tagged,
    gc::Gc,
};

// ============================================================================
// Type Definitions
// ============================================================================

/// Function pointer type - now just a raw pointer to allow varying signatures
pub type FuncPtr = *const u8;

#[derive(Debug, Clone, Trace)]
pub struct LispFunction(pub(crate) Gc<Function>);
impl_tagged_for_gc!(LispFunction, LispType::Function, Function);

#[derive(Debug, Clone, Trace)]
pub struct Function {
    #[no_trace]
    pub func_type: FunctionType,

    #[no_trace]
    pub signature: FunctionSignature,
}

#[derive(Debug, Clone, Trace)]
pub enum FunctionType {
    #[no_trace]
    Subr(SubrFn),
    Lambda(Closure),
}

#[derive(Debug, Clone, Copy)]
pub struct SubrFn {
    name: Ident,
    func_id: FuncId,
    func_ptr: Option<FuncPtr>,
}

#[derive(Debug, Clone, Trace)]
pub struct Closure {
    pub captures: FxHashMap<Ident, Object>,
    #[no_trace]
    func_id: FuncId,
    #[no_trace]
    func_ptr: Option<FuncPtr>,
}

#[allow(unused)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct FunctionSignature {
    /// minimal argument number required
    pub normal: u8,
    pub optional: u8,
    /// whether this function accepts variable args, like `&rest`
    pub rest: bool,
}

impl FunctionSignature {
    pub fn new(normal: u8, optional: u8, rest: bool) -> Self {
        Self {
            normal,
            optional,
            rest,
        }
    }

    pub fn from_args(args: &Args) -> Self {
        let normal = args.normal.len() as u8;
        let optional = args.optional.as_ref().map_or(0, |args| args.len() as u8);
        let rest = args.rest.is_some();
        Self::new(normal, optional, rest)
    }
}

// ============================================================================
// Function Implementation
// ============================================================================

impl Function {
    /// Create a new closure function
    pub fn new_closure(func_id: FuncId, signature: FunctionSignature) -> Self {
        let closure = Closure {
            captures: FxHashMap::default(),
            func_id,
            func_ptr: None,
        };
        Self {
            func_type: FunctionType::Lambda(closure),
            signature,
        }
    }

    /// Create a new built-in function (subr)
    pub fn new_subr(
        func_id: FuncId,
        name: &str,
        func_ptr: FuncPtr,
        signature: FunctionSignature,
    ) -> Self {
        let name = name.into();
        let subr = SubrFn {
            name,
            func_id,
            func_ptr: Some(func_ptr),
        };
        Self {
            func_type: FunctionType::Subr(subr),
            signature,
        }
    }

    /// Get the function ID
    pub fn func_id(&self) -> FuncId {
        match &self.func_type {
            FunctionType::Subr(subr) => subr.func_id,
            FunctionType::Lambda(closure) => closure.func_id,
        }
    }

    /// Set the function pointer
    pub fn set_func_ptr(&mut self, func_ptr: FuncPtr) {
        match &mut self.func_type {
            FunctionType::Subr(subr) => subr.func_ptr = Some(func_ptr),
            FunctionType::Lambda(closure) => closure.func_ptr = Some(func_ptr),
        }
    }

    /// Get the function pointer
    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        match &self.func_type {
            FunctionType::Subr(subr) => subr.func_ptr,
            FunctionType::Lambda(closure) => closure.func_ptr,
        }
    }

    /// Get the function signature
    pub fn signature(&self) -> FunctionSignature {
        self.signature
    }

    /// Check if argument count is valid and return calling convention info
    /// Returns: (is_valid, total_args, use_trampoline)
    pub fn check_args(&self, argc: usize) -> (bool, usize, bool) {
        let sig = &self.signature;
        let min_args = sig.normal as usize;
        let max_args = if sig.rest {
            usize::MAX
        } else {
            (sig.normal + sig.optional) as usize
        };

        let is_valid = argc >= min_args && argc <= max_args;
        let total_args = (sig.normal + sig.optional) as usize + if sig.rest { 2 } else { 0 };
        let use_trampoline = sig.rest || total_args > 8;

        (is_valid, total_args, use_trampoline)
    }

    /// Run the function with given arguments
    pub fn run(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        let func_ptr = self
            .get_func_ptr()
            .ok_or(RuntimeError::internal_error("no function pointer"))?;

        let (is_valid, total_args, use_trampoline) = self.check_args(args.len());
        if !is_valid {
            return Err(RuntimeError::WrongNumberOfArgs {
                expected: self.signature.normal as usize,
                actual: args.len(),
            });
        }

        if use_trampoline {
            // Use current trampoline style for >8 args
            let func: unsafe extern "C" fn(*const Object, u64, *const Environment) -> Object =
                unsafe { std::mem::transmute(func_ptr) };
            let result = unsafe { func(args.as_ptr(), args.len() as u64, env) };
            Ok(result)
        } else {
            debug_assert!(total_args <= 8);

            if args.len() < total_args {
                return Err(RuntimeError::internal_error(
                    "missing argument padding for direct call",
                ));
            }

            let env_raw = env as *const Environment as i64;

            macro_rules! gen_func_type {
                (@type $_:tt) => {
                    i64
                };
            }

            macro_rules! call_direct_case {
                ($($idx:tt),+) => {{
                    type FnTy = unsafe extern "C" fn($(gen_func_type!(@type $idx)),+, i64) -> i64;
                    let func: FnTy = unsafe { std::mem::transmute(func_ptr) };
                    let value = unsafe { func($(args[$idx].0 as i64),+, env_raw) };
                    Ok(Object(value as u64))
                }};
            }

            match total_args {
                0 => {
                    type FnTy = unsafe extern "C" fn(i64) -> i64;
                    let func: FnTy = unsafe { std::mem::transmute(func_ptr) };
                    let value = unsafe { func(env_raw) };
                    Ok(Object(value as u64))
                }
                1 => call_direct_case!(0),
                2 => call_direct_case!(0, 1),
                3 => call_direct_case!(0, 1, 2),
                4 => call_direct_case!(0, 1, 2, 3),
                5 => call_direct_case!(0, 1, 2, 3, 4),
                6 => call_direct_case!(0, 1, 2, 3, 4, 5),
                7 => call_direct_case!(0, 1, 2, 3, 4, 5, 6),
                8 => call_direct_case!(0, 1, 2, 3, 4, 5, 6, 7),
                _ => unreachable!("direct call arity exceeds supported range"),
            }
        }
    }

    /// Get mutable reference to function type
    pub fn get_func_type_mut(&mut self) -> &mut FunctionType {
        &mut self.func_type
    }
}

impl FunctionType {
    pub fn as_closure_mut(&mut self) -> Option<&mut Closure> {
        if let FunctionType::Lambda(func) = self {
            Some(func)
        } else {
            None
        }
    }
}

// ============================================================================
// LispFunction Implementation
// ============================================================================

impl LispFunction {
    /// Create a new closure function
    pub fn new_closure(func_id: FuncId, signature: FunctionSignature) -> Self {
        let inner = Gc::new(Function::new_closure(func_id, signature));
        Self(inner)
    }

    /// Create a new built-in function (subr)
    pub fn new_subr(
        func_id: FuncId,
        name: &str,
        func_ptr: FuncPtr,
        signature: FunctionSignature,
    ) -> Self {
        let inner = Gc::new(Function::new_subr(func_id, name, func_ptr, signature));
        Self(inner)
    }

    /// Get the function ID
    pub fn func_id(&self) -> FuncId {
        self.0.get().func_id()
    }

    /// Get mutable reference to function type
    pub fn get_func_type_mut(&self) -> &mut FunctionType {
        &mut self.0.get_mut().func_type
    }

    /// Get closure if this is a lambda function
    pub fn as_closure(&self) -> Option<&mut Closure> {
        match &mut self.0.get_mut().func_type {
            FunctionType::Lambda(closure) => Some(closure),
            _ => None,
        }
    }

    /// Set the function pointer
    pub fn set_func_ptr(&self, func_ptr: FuncPtr) {
        self.0.get_mut().set_func_ptr(func_ptr)
    }

    /// Get the function pointer
    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        self.0.get().get_func_ptr()
    }

    /// Check if argument count is valid and return calling convention info
    /// Returns: (is_valid, total_args, use_trampoline)
    pub fn check_args(&self, argc: usize) -> (bool, usize, bool) {
        self.0.get().check_args(argc)
    }

    /// Run the function with given arguments
    pub fn run(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        self.0.get().run(args, env)
    }

    /// Get the function signature
    pub fn signature(&self) -> FunctionSignature {
        self.0.get().signature()
    }
}
