use proc_macros::Trace;
use rustc_hash::FxBuildHasher;

use crate::{
    core::{
        error::{RuntimeError, RuntimeResult},
        ident::Ident,
        object::{Object, ObjectRef},
        symbol::{Symbol, SymbolCell, SymbolMap},
    },
    gc::Gc,
};

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub struct Bind {
    #[no_trace]
    pub symbol: Symbol,
    pub value: Object,
}

use rustc_hash::FxHashMap;
#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub enum SpecStackItem {
    Bind(Bind),
    /// mark for start of a lexical envrionment
    LexicalMark,
    Unwind,
}

use std::cell::RefCell;

#[derive(Debug, Default)]
pub struct Environment {
    /// the obarray
    pub symbol_map: Gc<SymbolMap>,

    pub spec_stack: Gc<Vec<SpecStackItem>>,

    pub stack_map: StackMap,
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
    DYN_TOP.with(|top| top.borrow().get(&symbol).copied()).and_then(|idx| {
        DYN_STACK.with(|stack| stack.borrow().get(idx).map(|n| n.value.clone()))
    })
}

fn tls_bind(symbol: Symbol, value: Object) {
    let prev = DYN_TOP.with(|m| m.borrow().get(&symbol).copied());
    let idx = DYN_STACK.with(|s| {
        let mut s = s.borrow_mut();
        let idx = s.len();
        s.push(BindNode { symbol, value, prev });
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
    let Some(mark) = mark else { return; };
    DYN_STACK.with(|s| {
        let mut s = s.borrow_mut();
        while s.len() > mark {
            if let Some(node) = s.pop() {
                // Unroot the value
                env.pop_stackmap(&node.value);
                // Restore top index
                DYN_TOP.with(|m| {
                    let mut m = m.borrow_mut();
                    if let Some(prev) = node.prev { m.insert(node.symbol, prev); } else { m.remove(&node.symbol); }
                });
            }
        }
    });
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
        self.symbol_map.get().get_symbol_cell_with(symbol, |cell| {
            match load_function_cell {
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
            }
        })
    }

    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<SymbolCell> {
        self.symbol_map.get().get_symbol_cell(symbol)
    }

    pub fn get_or_init_symbol(&self, symbol: Symbol) -> SymbolCell {
        self.symbol_map.get_mut().get_or_init_symbol(symbol)
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
        self.symbol_map
            .get()
            .get_symbol_cell(symbol)
            .map(|cell| cell.special)
            .unwrap_or(false)
    }

    /// declare a `defvar` or `defconst`
    fn declare_var(&self, ident: Ident) -> Symbol { self.symbol_map.get_mut().intern(ident, true).0 }
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
