use rustc_hash::FxBuildHasher;

use crate::{
    core::{
        error::{RuntimeError, RuntimeResult},
        ident::Ident,
        object::{Object, ObjectRef},
        symbol::{Symbol, SymbolCell, SymbolMap},
        tag::TAG_SPEC_STACK,
    },
    gc::{Gc, HeaderedObject, Trace, Visitor},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bind {
    pub symbol: Symbol,
    pub value: Object,
}

use rustc_hash::FxHashMap;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecStackItem {
    Bind(Bind),
    /// mark for start of a lexical envrionment
    LexicalMark,
    Unwind,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SpecStack(pub Vec<SpecStackItem>);

impl crate::gc::Tagged for SpecStack {
    const TAG: u8 = TAG_SPEC_STACK;
}

unsafe impl HeaderedObject for SpecStack {}

unsafe impl Trace for Bind {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        unsafe { self.value.trace(visitor) };
    }
}

unsafe impl Trace for SpecStackItem {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        if let SpecStackItem::Bind(bind) = self {
            unsafe { bind.trace(visitor) };
        }
    }
}

unsafe impl Trace for SpecStack {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        // Dynamic binding entries keep tagged values alive while bound.
        for item in &self.0 {
            unsafe { item.trace(visitor) };
        }
    }
}

use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::{cell::RefCell, ptr::NonNull};

#[derive(Debug)]
pub struct Environment {
    /// the obarray
    symbol_map: crate::gc::GcRooted<SymbolMap>,

    spec_stack: crate::gc::GcRooted<SpecStack>,

    pub stack_map: StackMap,

    /// Cached mutator pointer used by generated code for fast safepoint polls.
    ///
    /// The pointer comes from the thread manager/TLS-owned `Arc<MutatorThread>`.
    /// JIT code treats this field as an opaque address and loads fields through
    /// the offset constants on `MutatorThread`.
    pub thread: *const crate::gc::MutatorThread,
}

/// Dynamic binding entry for one global special variable.
///
/// `index` is the global symbol-map index. `obj` is the dynamically bound
/// value that should shadow the global value while the binding record is live.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynamicBinding {
    pub index: usize,
    pub obj: Object,
}

/// Header for a caller-owned dynamic binding segment.
///
/// JIT code will place this header and `len` following `DynamicBinding` records
/// in its own stack frame.
#[repr(C)]
#[derive(Debug)]
pub struct DynamicEnv {
    pub len: usize,
    pub parent: *mut DynamicEnv,
    pub bindings: [DynamicBinding; 0],
}

pub struct Dynamic {
    raw: Option<NonNull<DynamicEnv>>,
}

pub struct RootSet<'gc> {
    dynamics: PhantomData<&'gc DynamicEnv>,
}

/// Runtime environment view passed through Lisp calls.
///
/// `global` owns process/thread runtime state such as the symbol map, while
/// `dynamic` points at the caller's current special-binding chain.
#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub global: &'a Environment,
    pub dynamic: &'a DynamicEnv,
}

impl DynamicEnv {
    /// Walk this dynamic environment chain looking for the newest binding of a
    /// global special-variable index.
    ///
    /// # Safety
    ///
    /// Every `DynamicEnv` in the parent chain must point at a live stack record
    /// with at least `len` trailing `DynamicBinding` entries.
    pub unsafe fn lookup(&self, index: usize) -> Option<Object> {
        let mut env = self as *const DynamicEnv;
        while !env.is_null() {
            let current = unsafe { &*env };
            let bindings = current.bindings.as_ptr();

            for offset in (0..current.len).rev() {
                let binding = unsafe { *bindings.add(offset) };
                if binding.index == index {
                    return Some(binding.obj);
                }
            }

            env = current.parent;
        }

        None
    }
}

impl Environment {
    /// Offset consumed by generated code; keep this in sync with `Environment`.
    pub const THREAD_OFFSET: usize = std::mem::offset_of!(Self, thread);
}

impl Environment {
    /// The caller's context owns the environment roots and must outlive it.
    pub fn new(ctx: &mut crate::gc::GcContext) -> Self {
        let symbol_map = ctx.alloc::<SymbolMap>(());
        let symbol_map = ctx.root_gc(symbol_map);
        let spec_stack = ctx.alloc::<SpecStack>(());
        let spec_stack = ctx.root_gc(spec_stack);
        Self {
            symbol_map,
            spec_stack,
            stack_map: StackMap::default(),
            thread: std::sync::Arc::as_ptr(&crate::gc::current_thread()),
        }
    }

    pub fn symbol_map(&self) -> Gc<SymbolMap> {
        self.symbol_map.to_gc(&crate::gc::GcContext::new())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncCellType {
    Function,
    Macro,
}

impl FuncCellType {
    fn ident(self) -> Ident {
        match self {
            FuncCellType::Function => Ident::special().function,
            FuncCellType::Macro => Ident::special().mcro,
        }
    }

    pub fn from_num(n: u64) -> Option<Self> {
        match n {
            0 => None,
            1 => Some(Self::Function),
            2 => Some(Self::Macro),
            _ => unreachable!(),
        }
    }
}

thread_local! {
    static DYN_STACK: std::cell::RefCell<Vec<BindNode>> = std::cell::RefCell::new(Vec::new());
    static DYN_TOP: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, usize>> = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    static DYN_MARKS: std::cell::RefCell<Vec<usize>> = std::cell::RefCell::new(Vec::new());
}

#[derive(Debug, Clone)]
struct BindNode {
    symbol: Symbol,
    value: Object,
    prev: Option<usize>,
}

fn tls_lookup(symbol: Symbol) -> Option<Object> {
    DYN_TOP
        .with(|top| top.borrow().get(&symbol).copied())
        .and_then(|idx| DYN_STACK.with(|stack| stack.borrow().get(idx).map(|n| n.value.clone())))
}

fn tls_bind(symbol: Symbol, value: Object) {
    let prev = DYN_TOP.with(|m| m.borrow().get(&symbol).copied());
    let idx = DYN_STACK.with(|s| {
        let mut s = s.borrow_mut();
        let idx = s.len();
        s.push(BindNode {
            symbol,
            value,
            prev,
        });
        idx
    });
    DYN_TOP.with(|m| {
        let mut m = m.borrow_mut();
        m.insert(symbol, idx);
    });
}

fn tls_mark() {
    let depth = DYN_STACK.with(|s| s.borrow().len());
    DYN_MARKS.with(|marks| marks.borrow_mut().push(depth));
}

fn tls_unbind_to_mark(env: &Environment) {
    let mark = DYN_MARKS.with(|marks| marks.borrow_mut().pop());
    let Some(mark) = mark else {
        return;
    };
    DYN_STACK.with(|s| {
        let mut s = s.borrow_mut();
        while s.len() > mark {
            if let Some(node) = s.pop() {
                // Unroot the value
                env.pop_stackmap(&node.value);
                // Restore top index
                DYN_TOP.with(|m| {
                    let mut m = m.borrow_mut();
                    if let Some(prev) = node.prev {
                        m.insert(node.symbol, prev);
                    } else {
                        m.remove(&node.symbol);
                    }
                });
            }
        }
    });
}

// A fake guard that mimics a lock-free map read guard, returning a value
// view without cloning or affecting refcounts. The inner Object is placed
// in ManuallyDrop so dropping the guard won't decrement RC. The PhantomData
// ties the guard lifetime to the environment/map access even though no
// actual borrow is held.
#[derive(Debug)]
pub struct EnvValueGuard<'a> {
    value: ManuallyDrop<Object>,
    _marker: PhantomData<&'a Object>,
}

impl<'a> EnvValueGuard<'a> {
    pub fn as_ref(&self) -> &Object {
        &self.value
    }
    pub fn raw_i64(&self) -> i64 {
        self.value.0 as i64
    }
}

impl Environment {
    /// load symbol's value from the global symbol table. Neglect the stacked lexical values.
    /// this can be used to load function cells, as they are always global;
    /// or as the fallback for dynamic binding symbols, i.e. special symbol that created
    /// with `defvar` or `defconst`.
    pub fn load_symbol_with<F, T>(
        &self,
        symbol: Symbol,
        load_function_cell: Option<FuncCellType>,
        job: F,
    ) -> RuntimeResult<T>
    where
        F: FnOnce(&Object) -> RuntimeResult<T>,
    {
        // Fast-path: thread-local dynamic binding lookup for values
        if load_function_cell.is_none() {
            if let Some(val) = tls_lookup(symbol) {
                return job(&val);
            }
        }

        // Fall back to global environment
        self.symbol_map()
            .as_ref()
            .get_symbol_cell_with(symbol, |cell| match load_function_cell {
                Some(ty) => {
                    let ObjectRef::Cons(cons) = cell.func.as_ref() else {
                        return Err(RuntimeError::wrong_type("cons", cell.func.get_tag()));
                    };
                    let ObjectRef::Symbol(marker) = cons.car().as_ref() else {
                        return Err(RuntimeError::wrong_type("symbol", cons.car().get_tag()));
                    };

                    if marker.ident() == ty.ident() {
                        let val = cons.cdr();
                        return job(val);
                    } else {
                        return Err(RuntimeError::internal_error("not a function"));
                    }
                }
                None => {
                    tracing::debug!("loaded value from global: {:?}", cell.value);
                    return job(&cell.value);
                }
            })
    }

    /// Load a symbol's binding and return a guard that views the value without
    /// inc/dec of refcounts. When `load_function_cell` is Some, returns the
    /// function cell's payload (cdr) after verifying marker.
    pub fn load_symbol_guard<'a>(
        &'a self,
        symbol: Symbol,
        load_function_cell: Option<FuncCellType>,
    ) -> RuntimeResult<EnvValueGuard<'a>> {
        // Fast-path: thread-local dynamic binding lookup for values
        if load_function_cell.is_none() {
            if let Some(idx) = DYN_TOP.with(|top| top.borrow().get(&symbol).copied()) {
                if let Some(raw) =
                    DYN_STACK.with(|stack| stack.borrow().get(idx).map(|n| n.value.0))
                {
                    return Ok(EnvValueGuard {
                        value: ManuallyDrop::new(Object(raw)),
                        _marker: PhantomData,
                    });
                }
            }
        }

        let symbol_map = self.symbol_map();
        let cell_ref = match symbol_map.as_ref().get_symbol_cell_ref(symbol) {
            Some(cell) => cell,
            None => return Err(RuntimeError::unbound_symbol(symbol)),
        };

        match load_function_cell {
            Some(ty) => {
                let ObjectRef::Cons(cons) = cell_ref.func.as_ref() else {
                    return Err(RuntimeError::wrong_type("cons", cell_ref.func.get_tag()));
                };
                let ObjectRef::Symbol(marker) = cons.car().as_ref() else {
                    return Err(RuntimeError::wrong_type("symbol", cons.car().get_tag()));
                };
                if marker.ident() != ty.ident() {
                    return Err(RuntimeError::internal_error("not a function"));
                }
                let val = cons.cdr();
                Ok(EnvValueGuard {
                    value: ManuallyDrop::new(Object(val.0)),
                    _marker: PhantomData,
                })
            }
            None => {
                tracing::debug!("loaded value from global(ref): {:?}", cell_ref.value);
                Ok(EnvValueGuard {
                    value: ManuallyDrop::new(Object(cell_ref.value.0)),
                    _marker: PhantomData,
                })
            }
        }
    }

    /// Load a symbol through an explicit dynamic environment.
    ///
    /// Function cells ignore dynamic binding. Value cells consult `dynamic`
    /// only after the global symbol cell is known to be special.
    pub fn load_symbol_guard_with_dynamic<'a>(
        &'a self,
        symbol: Symbol,
        load_function_cell: Option<FuncCellType>,
        dynamic: Option<&DynamicEnv>,
    ) -> RuntimeResult<EnvValueGuard<'a>> {
        let symbol_map = self.symbol_map();
        let cell_ref = match symbol_map.as_ref().get_symbol_cell_ref(symbol) {
            Some(cell) => cell,
            None => return Err(RuntimeError::unbound_symbol(symbol)),
        };

        match load_function_cell {
            Some(ty) => {
                let ObjectRef::Cons(cons) = cell_ref.func.as_ref() else {
                    return Err(RuntimeError::wrong_type("cons", cell_ref.func.get_tag()));
                };
                let ObjectRef::Symbol(marker) = cons.car().as_ref() else {
                    return Err(RuntimeError::wrong_type("symbol", cons.car().get_tag()));
                };
                if marker.ident() != ty.ident() {
                    return Err(RuntimeError::internal_error("not a function"));
                }
                let val = cons.cdr();
                Ok(EnvValueGuard {
                    value: ManuallyDrop::new(Object(val.0)),
                    _marker: PhantomData,
                })
            }
            None => {
                if cell_ref.special {
                    if let Some(dynamic) = dynamic {
                        if let Some(value) = unsafe { dynamic.lookup(cell_ref.name.data()) } {
                            return Ok(EnvValueGuard {
                                value: ManuallyDrop::new(value),
                                _marker: PhantomData,
                            });
                        }
                    }
                }

                tracing::debug!("loaded value from global(ref): {:?}", cell_ref.value);
                Ok(EnvValueGuard {
                    value: ManuallyDrop::new(Object(cell_ref.value.0)),
                    _marker: PhantomData,
                })
            }
        }
    }

    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<SymbolCell> {
        self.symbol_map().as_ref().get_symbol_cell(symbol)
    }

    pub fn get_or_init_symbol(&self, symbol: Symbol) -> SymbolCell {
        let mut symbol_map = self.symbol_map();
        symbol_map.as_mut().get_or_init_symbol(symbol)
    }

    pub fn push_stackmap(&self, obj: &Object) {
        self.stack_map.push(obj);
    }

    pub fn pop_stackmap(&self, obj: &Object) {
        self.stack_map.pop(obj);
    }

    pub fn push_spec_stack(&self, item: SpecStackItem) {
        if let SpecStackItem::LexicalMark = item {
            tls_mark();
        }
    }

    pub fn push_special_symbol(&self, symbol: Symbol, value: Object) -> RuntimeResult<()> {
        // Push dynamic binding in thread-local stack and root it
        tls_bind(symbol, value.clone());
        self.push_stackmap(&value);
        Ok(())
    }

    pub fn pop_lexical(&self) -> RuntimeResult<()> {
        tls_unbind_to_mark(self);
        Ok(())
    }

    pub fn is_special(&self, symbol: Symbol) -> bool {
        self.symbol_map()
            .as_ref()
            .get_symbol_cell(symbol)
            .map(|cell| cell.special)
            .unwrap_or(false)
    }

    /// declare a `defvar` or `defconst`
    fn declare_var(&self, ident: Ident) -> Symbol {
        let mut symbol_map = self.symbol_map();
        symbol_map.as_mut().intern(ident, true).0
    }
}

use scc::HashMap;
/// a pesudo stack map that tracks objects.
#[derive(Debug)]
pub struct StackMap {
    roots: HashMap<Object, u64, rustc_hash::FxBuildHasher>,
}

impl StackMap {
    pub fn new() -> Self {
        Self {
            roots: HashMap::with_capacity_and_hasher(128, FxBuildHasher::default()),
        }
    }

    pub fn push(&self, obj: &Object) {
        if obj.is_primitive() {
            return;
        }
        match self.roots.get_sync(obj) {
            Some(mut entry) => {
                let count = entry.get_mut();
                *count = *count + 1;
            }
            _ => {
                self.roots.insert_sync(obj.clone(), 1).unwrap();
            }
        }
    }

    pub fn pop(&self, obj: &Object) {
        if obj.is_primitive() {
            return;
        }
        let mut need_remove = false;
        if let Some(mut entry) = self.roots.get_sync(obj) {
            let count = entry.get_mut();
            if *count > 1 {
                *count = *count - 1;
            } else {
                need_remove = true;
            }
        }
        if need_remove {
            self.roots.remove_sync(obj);
        }
    }
}

impl Default for StackMap {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::number::LispInteger;
    use crate::core::tag::Tag;

    #[repr(C)]
    struct DynamicEnv1 {
        header: DynamicEnv,
        bindings: [DynamicBinding; 1],
    }

    #[test]
    fn dynamic_env_shadows_only_special_value_cells() {
        let _mutator = crate::gc::test_support::bind_nogc_mutator();
        let mut ctx = crate::gc::GcContext::new();
        let env = Environment::new(&mut ctx);
        let mut symbol_map = env.symbol_map();

        let special = symbol_map.as_mut().intern(Ident::from("special"), true).0;
        let normal = symbol_map.as_mut().intern(Ident::from("normal"), false).0;
        let global_special = LispInteger(10).tag();
        let dynamic_special = LispInteger(20).tag();
        let global_normal = LispInteger(30).tag();
        let dynamic_normal = LispInteger(40).tag();

        symbol_map.as_mut().set_value(special, global_special);
        symbol_map.as_mut().set_value(normal, global_normal);

        let dynamic = DynamicEnv1 {
            header: DynamicEnv {
                len: 1,
                parent: std::ptr::null_mut(),
                bindings: [],
            },
            bindings: [DynamicBinding {
                index: special.data(),
                obj: dynamic_special,
            }],
        };

        let special_value = env
            .load_symbol_guard_with_dynamic(special, None, Some(&dynamic.header))
            .unwrap();
        assert_eq!(*special_value.as_ref(), dynamic_special);

        let normal_dynamic = DynamicEnv1 {
            header: DynamicEnv {
                len: 1,
                parent: std::ptr::null_mut(),
                bindings: [],
            },
            bindings: [DynamicBinding {
                index: normal.data(),
                obj: dynamic_normal,
            }],
        };

        let normal_value = env
            .load_symbol_guard_with_dynamic(normal, None, Some(&normal_dynamic.header))
            .unwrap();
        assert_eq!(*normal_value.as_ref(), global_normal);
    }
}

impl crate::gc::ConstructObject for SpecStack {
    type Init<'a> = ();
    fn size_from_init(_init: &Self::Init<'_>) -> usize {
        std::mem::size_of::<crate::gc::Headered<Self>>()
    }
    fn size(&self) -> usize {
        std::mem::size_of::<crate::gc::Headered<Self>>()
    }
    unsafe fn init_fn(_init: Self::Init<'_>, _ctx: &crate::gc::GcContext) -> Self {
        Self::default()
    }
}
