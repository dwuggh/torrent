use crate::core::{
    symbol::{Symbol, INTERNED_SYMBOLS},
    value::Value,
};

// pub(crate) static INTERNED_SYMBOLS: OnceLock<std::sync::Mutex<SymbolMap>>;

#[derive(Debug, Clone)]
#[derive(Default)]
pub struct Environment {
    inner: u64,
}


impl Environment {
    // Global lookup using INTERNED_SYMBOLS; no per-env state yet.
    pub fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> Option<Value> {
        let map = INTERNED_SYMBOLS.map();
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
}
