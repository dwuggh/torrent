use crate::core::env::{DynamicEnv, Environment};
use crate::core::error::{RuntimeError, RuntimeResult};
use crate::core::indirect::Indirect;
use crate::core::object::{Object, nil, write_object_slot};
use crate::core::parser::expr::Args;
use crate::core::tag::TAG_FUNCTION;
use crate::gc::{
    ConstructObject, Gc, GcContext, Headered, HeaderedObject, RootedSlice, Trace, Visitor,
};
use cranelift_module::FuncId;
use std::ptr::NonNull;

pub const MAX_FIXED_ARGS: usize = 8;
pub const ARGV_ABI_MARKER: i64 = 0xffff;
pub type FuncPtr = *const u8;

#[derive(Debug, Clone)]
pub struct LispFunction(pub(crate) Gc<Function>);
impl_tagged_for_gc!(LispFunction, Function);

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Jit,
    Native,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionInit {
    pub signature: FunctionSignature,
    pub id: FuncId,
    pub code: FuncPtr,
    pub kind: FunctionKind,
    pub binding_len: usize,
}

#[repr(C)]
#[derive(Debug)]
pub struct Function {
    pub signature: FunctionSignature,
    kind: FunctionKind,
    id: FuncId,
    code: FuncPtr,
    binding_len: usize,
    bindings: [Object; 0],
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

impl ConstructObject for Function {
    type Init<'a> = FunctionInit;
    fn size_from_init(init: &FunctionInit) -> usize {
        Self::size_for_bindings(init.binding_len)
    }
    fn size(&self) -> usize {
        Self::size_for_bindings(self.binding_len)
    }
    unsafe fn init_fn(init: FunctionInit, _ctx: &GcContext) -> Self {
        Self {
            signature: init.signature,
            kind: init.kind,
            id: init.id,
            code: init.code,
            binding_len: init.binding_len,
            bindings: [],
        }
    }
}

unsafe impl HeaderedObject for Function {
    unsafe fn init_tail(ptr: NonNull<Self>) {
        // The allocator reserved the prefix and binding_len trailing words.
        // Initialize every slot before post_alloc makes the object visible.
        unsafe {
            let tail = std::ptr::addr_of_mut!((*ptr.as_ptr()).bindings).cast::<Object>();
            for index in 0..(*ptr.as_ptr()).binding_len {
                tail.add(index).write(nil());
            }
        }
    }
}

unsafe impl Trace for Function {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        for value in self.bindings() {
            visitor.visit_slot_address(crate::gc::mmtk::util::Address::from_ptr(
                value as *const Object,
            ));
        }
    }
}

impl Function {
    pub fn size_for_bindings(len: usize) -> usize {
        len.checked_mul(std::mem::size_of::<Object>())
            .and_then(|tail| std::mem::size_of::<Headered<Self>>().checked_add(tail))
            .expect("function allocation size overflow")
    }
    pub fn binding_len(&self) -> usize {
        self.binding_len
    }
    pub fn bindings(&self) -> &[Object] {
        // Only published Functions may be borrowed; their full tails are initialized.
        unsafe { std::slice::from_raw_parts(self.bindings.as_ptr(), self.binding_len) }
    }
    pub fn func_id(&self) -> FuncId {
        self.id
    }
    pub fn kind(&self) -> FunctionKind {
        self.kind
    }
    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        Some(self.code)
    }
    pub fn signature(&self) -> FunctionSignature {
        self.signature
    }
    pub fn binding(&self, index: usize) -> RuntimeResult<Object> {
        self.bindings()
            .get(index)
            .copied()
            .ok_or_else(|| RuntimeError::internal_error("lexical binding index out of bounds"))
    }
    pub fn load_lexical(&self, index: usize) -> RuntimeResult<Object> {
        let value = self.binding(index)?;
        Ok(if Indirect::is_object(value) {
            unsafe { Indirect::from_object(value) }.load()
        } else {
            value
        })
    }
    pub fn set_binding(&mut self, index: usize, value: Object) -> RuntimeResult<()> {
        if index >= self.binding_len {
            return Err(RuntimeError::internal_error(
                "lexical binding index out of bounds",
            ));
        }
        let owner = Object::from_heap_ref(&*self);
        // The checked index is within the initialized trailing allocation.
        let slot = unsafe { &mut *self.bindings.as_mut_ptr().add(index) };
        write_object_slot(owner, slot, value);
        Ok(())
    }
    pub fn store_lexical(&mut self, index: usize, value: Object) -> RuntimeResult<()> {
        let old = self.binding(index)?;
        if Indirect::is_object(old) {
            unsafe { Indirect::from_object_mut(old) }.store(value);
            Ok(())
        } else {
            self.set_binding(index, value)
        }
    }
    pub fn check_args(&self, argc: usize) -> (bool, usize, bool) {
        (
            self.signature.accepts_argc(argc),
            self.signature.fixed_param_count(),
            self.signature.use_argv_abi(),
        )
    }
    pub fn run(
        &self,
        args: &[Object],
        env: &Environment,
        ctx: &mut GcContext,
    ) -> RuntimeResult<Object> {
        let roots = ctx.root_slice(args);
        self.run_rooted(&roots, env, ctx)
    }

    pub fn run_rooted(
        &self,
        args: &RootedSlice,
        env: &Environment,
        ctx: &mut GcContext,
    ) -> RuntimeResult<Object> {
        let args: Vec<Object> = args.iter().map(|arg| *arg.get(ctx)).collect();
        self.run_unrooted(args.as_slice(), env, ctx)
    }

    /// Run the function with given arguments after the caller has handled any
    /// root publication required by the current Rust/JIT boundary.
    fn run_unrooted(
        &self,
        args: &[Object],
        env: &Environment,
        ctx: &mut GcContext,
    ) -> RuntimeResult<Object> {
        let dynamic = DynamicEnv {
            len: 0,
            parent: std::ptr::null_mut(),
            bindings: [],
        };
        self.run_unrooted_with_dynamic(args, env, &dynamic, ctx)
    }

    fn run_unrooted_with_dynamic(
        &self,
        args: &[Object],
        env: &Environment,
        dynamic: &DynamicEnv,
        ctx: &mut GcContext,
    ) -> RuntimeResult<Object> {
        let owner = ctx.root(Object::from_heap_ref(self));
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
                Object,
            ) -> Object = unsafe { std::mem::transmute(func_ptr) };
            let result = unsafe {
                func(
                    args.as_ptr(),
                    args.len() as u64,
                    env,
                    dynamic,
                    *owner.get(ctx),
                )
            };
            Ok(result)
        } else {
            debug_assert!(total_args <= MAX_FIXED_ARGS);

            let env_raw = env as *const Environment as i64;
            let dynamic_raw = dynamic as *const DynamicEnv as i64;
            let lexical_raw = owner.get(ctx).raw() as i64;

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
}

impl LispFunction {
    pub fn new(init: FunctionInit, ctx: &mut GcContext) -> Self {
        Self(ctx.alloc::<Function>(init))
    }
    pub fn new_subr(
        id: FuncId,
        code: FuncPtr,
        signature: FunctionSignature,
        ctx: &mut GcContext,
    ) -> Self {
        Self::new(
            FunctionInit {
                signature,
                id,
                code,
                kind: FunctionKind::Native,
                binding_len: 0,
            },
            ctx,
        )
    }
    pub fn func_id(&self) -> FuncId {
        self.0.as_ref().func_id()
    }
    pub fn get_func_ptr(&self) -> Option<FuncPtr> {
        self.0.as_ref().get_func_ptr()
    }
    pub fn signature(&self) -> FunctionSignature {
        self.0.as_ref().signature()
    }
    pub fn check_args(&self, argc: usize) -> (bool, usize, bool) {
        self.0.as_ref().check_args(argc)
    }
    pub fn run(
        &self,
        args: &[Object],
        env: &Environment,
        ctx: &mut GcContext,
    ) -> RuntimeResult<Object> {
        self.0.as_ref().run(args, env, ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionSignature;

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
}
