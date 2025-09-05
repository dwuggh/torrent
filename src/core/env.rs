use std::{
    collections::HashMap,
    sync::Arc,
};

use cranelift::prelude::Variable;
use proc_macros::Trace;

use crate::{
    core::{
        symbol::{Symbol, SymbolCell, INTERNED_SYMBOLS},
        value::Value,
    },
    gc::Gc,
};

// pub(crate) static INTERNED_SYMBOLS: OnceLock<std::sync::Mutex<SymbolMap>>;

pub struct Environment;

impl Default for Environment {
    fn default() -> Self {
        Self
    }
}

impl Environment {
    // Global lookup using INTERNED_SYMBOLS; no per-env state yet.
    pub fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> Option<Value> {
        let map = INTERNED_SYMBOLS.map();
        if let Some(cell) = map.get(&symbol.name) {
            let data = cell.data();
            return if load_function_cell { data.func } else { data.value };
        }
        None
    }
}

#[derive(Debug)]
pub struct LexicalScope {
    pub vars: HashMap<Symbol, Gc<SymbolCell>> 
}

pub type ParamsMap = HashMap<Symbol, Variable>;

#[derive(Debug, Clone, Trace)]
pub struct ParamsScope {
    #[no_trace]
    params: ParamsMap,
}

impl ParamsScope {
    pub fn new(params: ParamsMap) -> Self {
        Self {
            params,
        }
    }

    pub fn load_params(&self, symbol: Symbol) -> Option<Variable> {
        self.params.get(&symbol).copied()
    }
}

