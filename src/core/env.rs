use std::{
    collections::HashMap,
    sync::{atomic::AtomicU64, Arc, LazyLock, OnceLock},
};

use cranelift::prelude::Variable;
use proc_macros::Trace;

use crate::{
    core::{
        symbol::{Symbol, SymbolCell, INTERNED_SYMBOLS},
        value::Value,
    },
    gc::{Gc, Trace},
};

// pub(crate) static INTERNED_SYMBOLS: OnceLock<std::sync::Mutex<SymbolMap>>;

pub struct Environment(pub Gc<General>);

#[derive(Debug, Clone, Trace)]
pub enum Env {
    General(Gc<General>),
    // Lexical(Gc<LexicalScope>),
    Params(Arc<ParamsScope>),
}

#[derive(Debug, Clone, Default)]
pub struct General {
    // TODO Value or Gc<Value>?
    // TODO should we trace Symbol here?
    vars: HashMap<Symbol, Value>,

    parent: Option<Env>
}

unsafe impl Trace for General {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        for (k, v) in self.vars.iter() {
            v.trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for (k, v) in self.vars.iter_mut() {
            v.finalize();
        }
    }
}

impl Env {
    pub fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> Option<Value> {
        // self.inner.get().load_symbol(symbol, load_function_cell)
        match self {
            Env::General(general) => {
                general.get().load_symbol(symbol, load_function_cell)
            }
            Env::Params(params_scope) => {
                let var = params_scope.params.get(&symbol);
                todo!()
            }
        }
    }

    pub fn add_params(&self, params: HashMap<Symbol, Variable>) -> Self {
        Self::Params(Arc::new(ParamsScope::new(params )))
    }
}


impl General {
    // TODO
    pub fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> Option<Value> {
        // First check local variables
        if let Some(val) = self.vars.get(&symbol) {
            return Some(*val);
        }
        
        // Then check interned symbols
        let map = INTERNED_SYMBOLS.map();
        if let Some(cell) = map.get(&symbol.name) {
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

