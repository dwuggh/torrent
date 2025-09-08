use dashmap::mapref::one::RefMut;

use crate::core::{
    symbol::{Symbol, SymbolCell, SymbolMap, INTERNED_SYMBOLS},
    value::{LispValue, TaggedPtr, Value},
};

// pub(crate) static INTERNED_SYMBOLS: OnceLock<std::sync::Mutex<SymbolMap>>;

#[derive(Debug, Default)]
pub struct Environment {
    /// the obarray
    // TODO should this be GC'd? or make SymbolCell GC
    pub symbol_map: SymbolMap,
}

impl Environment {
    pub fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> Option<Value> {
        let mut data_ref = self.get_symbol_cell(symbol)?;
        let data = data_ref.value_mut().data();
        if load_function_cell {
            let LispValue::Cons(cons) = data.func.untag() else {
                return None;
            };
            let LispValue::Symbol(marker) = cons.car().untag() else {
                return None;
            };
            match marker.name() {
                "function" => Some(cons.cdr()),
                _ => return None,
            }
        } else {
            return Some(data.value);
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
