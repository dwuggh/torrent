use crate::core::error::{RuntimeError, RuntimeResult};
use smallvec::SmallVec;
use std::cell::UnsafeCell;

use crate::core::ident::Ident;
use crate::core::lexical::LexicalBindings;
use crate::core::parser::expr::Args;
use cranelift_module::FuncId;

use crate::core::env::{DynamicEnv, Environment};
use crate::{
    core::{object::Object, tag::TAG_FUNCTION},
    gc::{Gc, HeaderedObject, Trace, Visitor},
};

pub const MAX_FIXED_ARGS: usize = 8;
pub const ARGV_ABI_MARKER: i64 = 0xffff;

// ============================================================================
// Type Definitions
// ============================================================================

/// Function pointer type - now just a raw pointer to allow varying signatures
pub type FuncPtr = *const u8;

#[derive(Debug, Clone)]
pub struct LispFunction(pub(crate) Gc<Function>);
impl_tagged_for_gc!(LispFunction, Function);

#[derive(Debug)]
pub struct Function {
    pub func_type: FunctionType,

    pub signature: FunctionSignature,
}

#[derive(Debug)]
pub enum FunctionType {
    Subr(SubrFn),
    Lambda(Closure),
}

#[derive(Debug, Clone, Copy)]
pub struct SubrFn {
    name: Ident,
    func_id: FuncId,
    func_ptr: Option<FuncPtr>,
}

pub struct Closure {
    captures: UnsafeCell<LexicalBindings>,
    func_id: FuncId,
    func_ptr: Option<FuncPtr>,
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("captures", self.captures())
            .field("func_id", &self.func_id)
            .field("func_ptr", &self.func_ptr)
            .finish()
    }
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

    pub fn fixed_param_count(&self) -> usize {
        self.normal as usize + self.optional as usize
    }

    pub fn accepts_argc(&self, argc: usize) -> bool {
        let min_args = self.normal as usize;
        let max_args = if self.rest {
            usize::MAX
        } else {
            self.fixed_param_count()
        };

        argc >= min_args && argc <= max_args
    }

    pub fn use_argv_abi(&self) -> bool {
        self.rest || self.fixed_param_count() > MAX_FIXED_ARGS
    }
}

impl crate::gc::Tagged for Function {
    const TAG: u8 = TAG_FUNCTION;
}

unsafe impl HeaderedObject for Function {}

unsafe impl Trace for Function {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        if let FunctionType::Lambda(closure) = &self.func_type {
            unsafe { closure.trace(visitor) };
        }
    }
}

unsafe impl Trace for Closure {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        unsafe { self.captures().trace(visitor) };
    }
}

impl Closure {
    pub fn new(func_id: FuncId) -> Self {
        Self {
            captures: UnsafeCell::new(LexicalBindings::empty()),
            func_id,
            func_ptr: None,
        }
    }

    pub fn captures(&self) -> &LexicalBindings {
        unsafe { &*self.captures.get() }
    }

    pub fn captures_mut(&self) -> &mut LexicalBindings {
        unsafe { &mut *self.captures.get() }
    }

    pub fn lexical_ptr(&self) -> *mut LexicalBindings {
        self.captures.get()
    }

    pub fn reset_captures(&self, captures: LexicalBindings) {
        unsafe { *self.captures.get() = captures };
    }
}

// ============================================================================
// Function Implementation
// ============================================================================

impl Function {
    /// Create a new closure function
    pub fn new_closure(func_id: FuncId, signature: FunctionSignature) -> Self {
        let closure = Closure::new(func_id);
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

    pub fn lexical_ptr(&self) -> *mut LexicalBindings {
        match &self.func_type {
            FunctionType::Subr(_) => std::ptr::null_mut(),
            FunctionType::Lambda(closure) => closure.lexical_ptr(),
        }
    }

    pub fn load_lexical(&self, index: usize) -> RuntimeResult<Object> {
        let FunctionType::Lambda(closure) = &self.func_type else {
            return Err(RuntimeError::internal_error("subr has no lexical bindings"));
        };
        closure
            .captures()
            .load(index)
            .ok_or_else(|| RuntimeError::internal_error("lexical binding index out of bounds"))
    }

    pub fn store_lexical(&mut self, index: usize, value: Object) -> RuntimeResult<()> {
        let owner = Object::from_heap_ref(&*self);
        let FunctionType::Lambda(closure) = &self.func_type else {
            return Err(RuntimeError::internal_error("subr has no lexical bindings"));
        };
        closure.captures_mut().store_with_owner(owner, index, value);
        Ok(())
    }

    pub fn reset_lexical_bindings(&mut self, captures: LexicalBindings) -> RuntimeResult<()> {
        let FunctionType::Lambda(closure) = &self.func_type else {
            return Err(RuntimeError::internal_error("subr has no lexical bindings"));
        };
        closure.reset_captures(captures);
        Ok(())
    }

    /// Get the function signature
    pub fn signature(&self) -> FunctionSignature {
        self.signature
    }

    /// Check if argument count is valid and return calling convention info.
    ///
    /// Returns `(is_valid, fixed_param_count, use_argv_abi)`.
    pub fn check_args(&self, argc: usize) -> (bool, usize, bool) {
        let sig = &self.signature;
        let is_valid = sig.accepts_argc(argc);
        let total_args = sig.fixed_param_count();
        let use_argv_abi = sig.use_argv_abi();

        (is_valid, total_args, use_argv_abi)
    }

    pub fn run(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        if matches!(&self.func_type, FunctionType::Lambda(_)) {
            return match args.len() {
                0 => self.run_with_native_frame::<0>(args, env),
                1 => self.run_with_native_frame::<1>(args, env),
                2 => self.run_with_native_frame::<2>(args, env),
                3 => self.run_with_native_frame::<3>(args, env),
                4 => self.run_with_native_frame::<4>(args, env),
                5 => self.run_with_native_frame::<5>(args, env),
                6 => self.run_with_native_frame::<6>(args, env),
                7 => self.run_with_native_frame::<7>(args, env),
                8 => self.run_with_native_frame::<8>(args, env),
                _ => self.run_with_native_frame::<0>(args, env),
            };
        }

        self.run_unrooted(args, env)
    }

    fn run_with_native_frame<const N: usize>(
        &self,
        args: &[Object],
        env: &Environment,
    ) -> RuntimeResult<Object> {
        let thread = crate::gc::current_thread();
        let mut frame = crate::gc::NativeRootFrame::<N>::new(SmallVec::from_slice(args));
        let _guard = unsafe { crate::gc::push_native_root_frame(&thread, &mut frame) };
        self.run_unrooted(frame.slots.as_slice(), env)
    }

    /// Run the function with given arguments after the caller has handled any
    /// root publication required by the current Rust/JIT boundary.
    fn run_unrooted(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        let dynamic = DynamicEnv {
            len: 0,
            parent: std::ptr::null_mut(),
            bindings: [],
        };
        self.run_unrooted_with_dynamic(args, env, &dynamic)
    }

    fn run_unrooted_with_dynamic(
        &self,
        args: &[Object],
        env: &Environment,
        dynamic: &DynamicEnv,
    ) -> RuntimeResult<Object> {
        let func_ptr = self
            .get_func_ptr()
            .ok_or(RuntimeError::internal_error("no function pointer"))?;

        let (is_valid, total_args, use_argv_abi) = self.check_args(args.len());
        if !is_valid {
            return Err(RuntimeError::WrongNumberOfArgs {
                expected: self.signature.normal as usize,
                actual: args.len(),
            });
        }

        if use_argv_abi {
            // Use argv ABI for rest arguments or arities too large for fixed ABI.
            let func: unsafe extern "C" fn(
                *const Object,
                u64,
                *const Environment,
                *const DynamicEnv,
                *mut LexicalBindings,
            ) -> Object = unsafe { std::mem::transmute(func_ptr) };
            let result = unsafe {
                func(
                    args.as_ptr(),
                    args.len() as u64,
                    env,
                    dynamic,
                    self.lexical_ptr(),
                )
            };
            Ok(result)
        } else {
            debug_assert!(total_args <= MAX_FIXED_ARGS);

            let env_raw = env as *const Environment as i64;
            let dynamic_raw = dynamic as *const DynamicEnv as i64;
            let lexical_raw = self.lexical_ptr() as i64;

            macro_rules! gen_func_type {
                (@type $_:tt) => {
                    i64
                };
            }

            macro_rules! call_direct_case {
                ($($idx:tt),+) => {{
                    type FnTy = unsafe extern "C" fn($(gen_func_type!(@type $idx)),+, i64, i64, i64) -> i64;
                    let func: FnTy = unsafe { std::mem::transmute(func_ptr) };
                    let value = unsafe {
                        func(
                            $(args.get($idx).copied().unwrap_or_else(crate::core::object::nil).0 as i64),+,
                            env_raw,
                            dynamic_raw,
                            lexical_raw,
                        )
                    };
                    Ok(Object(value as u64))
                }};
            }

            match total_args {
                0 => {
                    type FnTy = unsafe extern "C" fn(i64, i64, i64) -> i64;
                    let func: FnTy = unsafe { std::mem::transmute(func_ptr) };
                    let value = unsafe { func(env_raw, dynamic_raw, lexical_raw) };
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
        self.0.as_ref().func_id()
    }

    /// Get mutable reference to function type
    pub fn get_func_type_mut(&self) -> &mut FunctionType {
        let mut inner = self.0.clone();
        let ptr = &mut inner.as_mut().func_type as *mut FunctionType;
        unsafe { &mut *ptr }
    }

    /// Get closure if this is a lambda function
    pub fn as_closure(&self) -> Option<&mut Closure> {
        match self.get_func_type_mut() {
            FunctionType::Lambda(closure) => Some(closure),
            _ => None,
        }
    }

    /// Set the function pointer
    pub fn set_func_ptr(&self, func_ptr: FuncPtr) {
        let mut inner = self.0.clone();
        inner.as_mut().set_func_ptr(func_ptr)
    }

    pub fn reset_lexical_bindings(&self, captures: LexicalBindings) -> RuntimeResult<()> {
        let mut inner = self.0.clone();
        inner.as_mut().reset_lexical_bindings(captures)
    }

    /// Get the function pointer
    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        self.0.as_ref().get_func_ptr()
    }

    /// Check if argument count is valid and return calling convention info.
    ///
    /// Returns `(is_valid, fixed_param_count, use_argv_abi)`.
    pub fn check_args(&self, argc: usize) -> (bool, usize, bool) {
        self.0.as_ref().check_args(argc)
    }

    /// Run the function with given arguments
    pub fn run(&self, args: &[Object], env: &Environment) -> RuntimeResult<Object> {
        self.0.as_ref().run(args, env)
    }

    /// Get the function signature
    pub fn signature(&self) -> FunctionSignature {
        self.0.as_ref().signature()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signature_uses_argv_abi_only_for_rest_or_large_fixed_arity() {
        assert!(!FunctionSignature::new(2, 0, false).use_argv_abi());
        assert!(!FunctionSignature::new(1, 7, false).use_argv_abi());
        assert!(FunctionSignature::new(9, 0, false).use_argv_abi());
        assert!(FunctionSignature::new(1, 0, true).use_argv_abi());
    }

    #[test]
    fn signature_accepts_missing_optional_arguments_without_padding() {
        let sig = FunctionSignature::new(1, 2, false);

        assert!(sig.accepts_argc(1));
        assert!(sig.accepts_argc(2));
        assert!(sig.accepts_argc(3));
        assert!(!sig.accepts_argc(0));
        assert!(!sig.accepts_argc(4));
    }

    #[test]
    fn check_args_reports_fixed_abi_param_count_for_optional_arguments() {
        let func_id =
            cranelift_module::FuncId::from_name(&cranelift_module::ModuleRelocTarget::User {
                namespace: 0,
                index: 0,
            });
        let func = Function::new_subr(
            func_id,
            "optional-test",
            std::ptr::null(),
            FunctionSignature::new(1, 2, false),
        );

        assert_eq!(func.check_args(1), (true, 3, false));
    }
}
