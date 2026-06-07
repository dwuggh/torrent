use std::{cell::{Cell, UnsafeCell}, ptr::NonNull, sync::Mutex};

use rustc_hash::FxHashMap;

use crate::core::{object::Object, symbol::{Symbol, SymbolCell}};

#[repr(C)]
pub struct FastVec {
    // Pointer to an array of SymbolCell*.
    pub cells_base: *mut SymbolCell,

    pub cells_len: usize,
}

impl FastVec {
    pub const CELLS_BASE_OFFSET: usize = std::mem::offset_of!(Self, cells_base);
    pub const CELLS_LEN_OFFSET: usize = std::mem::offset_of!(Self, cells_len);
}

struct DirtyRootSet {
    // Appended by the mutator. Contains RootSlotId entries.
    dirty_indices: Vec<usize>,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct Environment {
    pub symbol_map: NonNull<SymbolMap>,

    pub spec_stack: NonNull<SpecStack>,

    // reserved for lexical binding list.
    // perhaps separate this as a new argument
    pub lexical: Option<NonNull<LexicalBindingList>>

}

impl DirtyRootSet {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            dirty_indices: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn push(&mut self, id: usize) {
        self.dirty_indices.push(id);
    }
}

struct SymbolC {
    /// a lock for mutation.
    lock: Mutex<()>,
    /// The symbol this cell represents
    pub name: Symbol,
    /// Function binding (nil if unbound)
    /// TODO should this and value be atomic type?
    pub func: Object,
    /// Value binding (nil if unbound)
    pub value: Object,

    /// is this a global symbol. if not, it can be lexical or thread local
    global: bool,
    /// index to thread local symbol list.
    pub thread_local_value: usize,
    pub lexcial_value: Object,
    /// Whether this is a special symbol, i.e. use dynamic binding
    pub special: bool,

}

#[repr(C)]
pub struct SymbolMap {
    header: FastVec,
    entries: Vec<SymbolCell>,
    indices: FxHashMap<Symbol, usize>,

    dirty_roots: DirtyRootSet,
}

impl SymbolMap {
    #[inline]
    fn refresh_fast_header(&mut self) {
        self.header.cells_base = self.entries.as_mut_ptr();
        self.header.cells_len = self.entries.len();
    }

    /// doesn't test whether symbol is in this map
    pub fn add_symbol_cell(&mut self, symbol: Symbol, cell: SymbolCell) -> usize {
        let id = self.entries.len();

        // Ensure dirty structures are ready before publishing the new cell length.
        self.ensure_dirty_capacity_for(id + 1);

        self.entries.push(cell);

        // entries.push may have reallocated, and len changed.
        self.refresh_fast_header();

        self.indices.insert(symbol, id);
        id
    }

    fn ensure_dirty_capacity_for(&mut self, len: usize) {
        todo!()
    }
}


struct Env;

struct Handle<T> {
    inner: T
}

impl Env {
    fn alloc<'a, T>(&mut self) -> &'a Object {
        todo!()
    }
}

impl Handle {
    fn get<'gc>(&self, env: &'gc Env) -> &'gc Object {
        unsafe {
            std::mem::transmute(&self.inner)
        }
    }
}

fn f(handle: &Handle, env: &mut Env) {
    let obj = handle.get(env);
    let b = env.alloc();
    let obj = handle.get(env);
    // obj;
}

    
