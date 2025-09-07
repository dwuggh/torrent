use dashmap::mapref::one::RefMut;

use crate::core::{
    symbol::{Symbol, SymbolCell, SymbolMap, INTERNED_SYMBOLS},
    value::Value,
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
        let map = self.symbol_map.map();
        if let Some(cell) = map.get(&symbol) {
            let data = cell.data();
            return if load_function_cell {
                data.func
            } else {
                data.value
            };
        }
        None
    }

    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<RefMut<'_, Symbol, SymbolCell>> {
        let map = self.symbol_map.map();
        map.get_mut(&symbol)
    }
    pub fn get_or_init_symbol(&self, symbol: Symbol) -> RefMut<'_, Symbol, SymbolCell> {
        let map = self.symbol_map.map();
        match    map.entry(symbol) {
            dashmap::Entry::Occupied(occupied_entry) => {
                occupied_entry.into_ref()
            }
            dashmap::Entry::Vacant(vacant_entry) => {
                let text = symbol.name();
                let special = text.starts_with(':');
                vacant_entry.insert(SymbolCell::new(symbol, special))
            }
        }
    }
}
