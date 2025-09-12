use dashmap::mapref::one::RefMut;

use crate::core::{
    error::{RuntimeError, RuntimeResult},
    symbol::{Symbol, SymbolCell, SymbolMap},
    object::{nil, LispObject, ObjectRef, Object},
    TaggedPtr,
};

// pub(crate) static INTERNED_SYMBOLS: OnceLock<std::sync::Mutex<SymbolMap>>;

#[derive(Debug, Default)]
pub struct Environment {
    /// the obarray
    // TODO should this be GC'd? or make SymbolCell GC
    pub symbol_map: SymbolMap,
}

impl Environment {
    // returning value means increase its ref count.
    // I think this is the desired behaviour.
    // TODO An alternative is to only increase ref count when setting a value to another slot, seems more idiomatic, but it is not allowed in rust. so we have to inc ref here, and dec ref when it is dropped. If we still want to reduce this overhead, we have to return a i64.
    pub fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> RuntimeResult<Object> {
        let Some(mut data_ref) = self.get_symbol_cell(symbol) else {
            return Ok(nil());
        };
        let data = data_ref.value_mut().data();
        if load_function_cell {
            let ObjectRef::Cons(cons) = data.func.as_ref() else {
                return Err(RuntimeError::wrong_type("cons", data.func.get_tag()));
            };
            let ObjectRef::Symbol(marker) = cons.car().as_ref() else {
                return Err(RuntimeError::wrong_type("symbol", cons.car().get_tag()));
            };
            match marker.name() {
                "function" => Ok(cons.cdr().clone()),
                _ => return Err(RuntimeError::internal_error("function cell corrupted")),
            }
        } else {
            tracing::debug!("loaded value: {:?}", data.value);
            return Ok(data.value.clone());
        }
    }

    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<RefMut<'_, Symbol, SymbolCell>> {
        let map = self.symbol_map.map();
        map.get_mut(&symbol)
    }

    pub fn get_or_init_symbol(&self, symbol: Symbol) -> RefMut<'_, Symbol, SymbolCell> {
        let map = self.symbol_map.map();
        match map.entry(symbol) {
            dashmap::Entry::Occupied(occupied_entry) => occupied_entry.into_ref(),
            dashmap::Entry::Vacant(vacant_entry) => {
                let text = symbol.name();
                let special = text.starts_with(':');
                vacant_entry.insert(SymbolCell::new(symbol, special))
            }
        }
    }

    pub fn init_nil_t(&self) {
        let _nil = self.get_or_init_symbol(Symbol::from("nil"));
        let t = Symbol::from("nil");
        let mut t_cell = self.get_or_init_symbol(t);
        t_cell.value_mut().data().value = t.tag();
    }
}
