use std::{
    collections::HashMap,
    sync::{atomic::AtomicU64, Arc, LazyLock, OnceLock},
};

use proc_macros::Trace;

use crate::{
    core::{
        symbol::{Symbol, INTERNED_SYMBOLS},
        value::Value,
    },
    gc::{Gc, Trace},
};

// pub(crate) static INTERNED_SYMBOLS: OnceLock<std::sync::Mutex<SymbolMap>>;

#[derive(Debug, Clone, Trace)]
pub struct Env {
    inner: Gc<General>,
}

#[derive(Debug, Clone)]
struct General {
    // TODO Value or Gc<Value>?
    // TODO should we trace Symbol here?
    vars: HashMap<Symbol, Value>,
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
        self.inner.get().load_symbol(symbol, load_function_cell)
    }
}


impl General {
    // TODO
    fn load_symbol(&self, symbol: Symbol, load_function_cell: bool) -> Option<Value> {
        let map = INTERNED_SYMBOLS.read();
        self.vars
            .get(&symbol)
            .map(|v| *v)
            .or(map.get_index(symbol.index()).and_then(|(_, v)| {
                let data = v.data();
                if load_function_cell {
                    data.func
                } else {
                    data.value 
                }
            }))
    }
}

#[derive(Debug, Clone, Trace)]
struct LexicalScope {}


struct ParamsScope {
    params: HashMap<Symbol, cranelift::prelude::Variable>,
    parent: Env
}
// #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Trace)]
// struct Mark(u64);

// impl Mark {
//     pub fn new() -> Self {
//         static MARK: AtomicU64 = AtomicU64::new(0);
//         let ordering = std::sync::atomic::Ordering::SeqCst;
//         let val = MARK.load(ordering);
//         MARK.store(val + 1, ordering);
//         Mark(val)
//     }
// }

// impl Default for Mark {
//     fn default() -> Self {
//         Self::new()
//     }
// }
