use rustc_hash::FxBuildHasher;
use scc::hash_index::{Entry, OccupiedEntry};

use crate::core::{
    error::{RuntimeError, RuntimeResult},
    object::{Object, ObjectRef},
    symbol::{Symbol, SymbolCell, SymbolMap},
    tagged_ptr::TaggedObj,
};

#[derive(Debug, Default)]
pub struct Environment {
    /// the obarray
    // TODO should this be GC'd? or make SymbolCell GC
    pub symbol_map: SymbolMap,
    pub stack_map: StackMap,
}

impl Environment {
    pub fn load_symbol_with<F, T>(
        &self,
        symbol: Symbol,
        load_function_cell: bool,
        job: F,
    ) -> RuntimeResult<T>
    where
        F: FnOnce(&Object) -> RuntimeResult<T>,
    {
        self.symbol_map.get_symbol_cell_with(symbol, |cell| {
            let data = cell.data();
            if load_function_cell {
                let ObjectRef::Cons(cons) = data.func.as_ref() else {
                    return Err(RuntimeError::wrong_type("cons", data.func.get_tag()));
                };
                let ObjectRef::Symbol(marker) = cons.car().as_ref() else {
                    return Err(RuntimeError::wrong_type("symbol", cons.car().get_tag()));
                };

                let val = cons.cdr();
                return job(val);
                // match marker.name() {
                //     "function" => {
                //         let val = cons.cdr();
                //         return job(val);
                //     }
                //     _ => return Err(RuntimeError::internal_error("function cell corrupted")),
                // }
            } else {
                tracing::debug!("loaded value: {:?}", data.value);
                // let value = Object(data.value.0);
                return job(&data.value);
            }
        })
    }

    pub fn get_symbol_cell(
        &self,
        symbol: Symbol,
    ) -> Option<scc::hash_index::OccupiedEntry<'_, Symbol, SymbolCell, FxBuildHasher>> {
        let map = self.symbol_map.map();
        map.get_sync(&symbol)
    }

    pub fn get_or_init_symbol(
        &self,
        symbol: Symbol,
    ) -> OccupiedEntry<'_, Symbol, SymbolCell, FxBuildHasher> {
        let map = self.symbol_map.map();
        match map.entry_sync(symbol) {
            Entry::Occupied(occupied_entry) => occupied_entry,
            Entry::Vacant(vacant_entry) => {
                let text = symbol.name();
                let special = text.starts_with(':');
                vacant_entry.insert_entry(SymbolCell::new(symbol, special))
            }
        }
    }

    pub fn init_nil_t(&self) {
        let _nil = self.get_or_init_symbol(Symbol::from("nil"));
        let t = Symbol::from("nil");
        let t_cell = self.get_or_init_symbol(t);
        t_cell.data().value = t.tag();
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
