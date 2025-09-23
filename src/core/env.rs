use std::sync::{Arc, Mutex};

use proc_macros::Trace;
use rustc_hash::{FxBuildHasher, FxHashMap};

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
    pub ident: Ident,
    pub value: Object,
}

#[derive(Debug, Default)]
pub struct Environment {
    /// the obarray
    // TODO should this be GC'd? or make SymbolCell GC
    pub symbol_map: SymbolMap,

    pub stack: Gc<Vec<Bind>>,

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
        self.symbol_map.get_symbol_cell_with(symbol, |cell| {
            let data = cell.data();
            match load_function_cell {
                Some(ty) => {
                    let ObjectRef::Cons(cons) = data.func.as_ref() else {
                        return Err(RuntimeError::wrong_type("cons", data.func.get_tag()));
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
                    tracing::debug!("loaded value: {:?}", data.value);
                    // let value = Object(data.value.0);
                    return job(&data.value);
                }
            }
        })
    }

    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<&SymbolCell> {
        self.symbol_map.get_symbol_cell(symbol)
    }

    pub fn get_or_init_symbol(&self, symbol: Symbol) -> &SymbolCell {
        self.symbol_map.get_or_init_symbol(symbol)
    }

    pub fn push_stackmap(&self, obj: &Object) {
        self.stack_map.push(obj);
    }

    pub fn pop_stackmap(&self, obj: &Object) {
        self.stack_map.pop(obj);
    }

    pub fn push_stack(&self, var: Bind) {
        self.stack.get_mut().push(var);
    }

    pub fn pop_stack(&self) {
        self.stack.get_mut().pop();
    }

    pub fn is_special(&self, symbol: Symbol) -> bool {
        self.symbol_map
            .get_symbol_cell(symbol)
            .map(|cell| cell.data().special)
            .unwrap_or(false)
    }

    pub fn load_symbol_value_with<T, F: FnOnce(&Object) -> RuntimeResult<T>>(
        &self,
        symbol: Symbol,
        job: F,
    ) -> RuntimeResult<T> {
        let is_special = self.is_special(symbol);
        if is_special {
            self.find_in_stack_with(symbol, job)
        } else {
            self.load_symbol_with(symbol, None, job)
        }
    }

    /// find a symbol value in stack
    fn find_in_stack_with<T, F: FnOnce(&Object) -> RuntimeResult<T>>(
        &self,
        symbol: Symbol,
        job: F,
    ) -> RuntimeResult<T> {
        let stack = self.stack.get();
        let mut i = stack.len();
        let ident = symbol.ident();
        while i > 0 {
            i = i - 1;
            let val = &stack[i];
            if val.ident == ident {
                return job(&val.value);
            }
        }
        self.load_symbol_with(symbol, None, job)
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
        if let Some(mut entry) = self.roots.get_sync(obj) {
            let count = entry.get_mut();
            *count = *count + 1;
        } else {
            self.roots.insert_sync(obj.clone(), 1).unwrap();
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
