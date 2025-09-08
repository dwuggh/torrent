use std::marker::PhantomData;
use std::sync::LazyLock;

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

use dashmap::DashMap;
use proc_macros::Trace;

use crate::core::compiler::scope::Val;
use crate::core::ident::Ident;
use crate::core::value::nil;
use crate::gc::Trace;
use crate::{
    core::value::{TaggedPtr, Value},
    gc::Gc,
};

use super::value::LispType;

pub static INTERNED_SYMBOLS: LazyLock<SymbolMap> = LazyLock::new(|| SymbolMap::with_capacity(100));

#[derive(Debug, Default)]
pub struct SymbolMap {
    map: DashMap<Symbol, SymbolCell>,
}

unsafe impl Trace for SymbolMap {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        for val in self.map.iter() {
            val.trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for mut val in self.map.iter_mut() {
            val.finalize();
        }
    }
}

impl SymbolMap {
    pub fn with_capacity(size: usize) -> Self {
        let map = DashMap::with_capacity(size);
        Self { map }
    }

    pub fn map(&self) -> &DashMap<Symbol, SymbolCell> {
        &self.map
    }

    pub fn intern(&self, symbol: Symbol, special: bool) -> Symbol {
        // Ensure the symbol cell exists in the map
        if !self.map.contains_key(&symbol) {
            self.map.insert(symbol, SymbolCell::new(symbol, special));
        }
        symbol
    }
}

#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Trace)]
pub struct Symbol {
    #[no_trace]
    pub name: Ident,
    phantom: PhantomData<SymbolCell>,
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Symbol").field("name", &self.name()).finish()
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl From<Ident> for Symbol {
    fn from(value: Ident) -> Self {
        Self::new(value)
    }
}

impl From<&Ident> for Symbol {
    fn from(value: &Ident) -> Self {
        Self::new(*value)
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Self::new(value.into())
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        Self::new(value.into())
    }
}

impl Symbol {
    pub fn new(name: Ident) -> Self {
        Self {
            name,
            phantom: PhantomData,
        }
    }

    pub fn name<'a>(&self) -> &'a str {
        self.name.text()
    }
}

impl TaggedPtr for Symbol {
    const TAG: LispType = LispType::Symbol;

    unsafe fn cast(val: u64) -> Self {
        Symbol {
            name: val.into(),
            phantom: PhantomData,
        }
    }

    unsafe fn get_untagged_data(self) -> u64 {
        self.name.into()
    }
}

#[derive(Debug, Trace, Clone, Copy)]
pub struct SymbolCellData {
    #[no_trace]
    pub name: Symbol,
    #[no_trace]
    pub interned: bool,
    #[no_trace]
    pub special: bool,
    pub func: Value,
    pub value: Value,
}

#[derive(Debug, Trace, Clone)]
pub struct SymbolCell(pub Gc<SymbolCellData>);

impl SymbolCellData {
    fn new(name: Symbol, special: bool) -> Self {
        Self {
            name,
            interned: true,
            func: nil(),
            value: nil(),
            special,
        }
    }
}

impl SymbolCell {
    pub fn new(sym: Symbol, special: bool) -> Self {
        SymbolCell(Gc::new(SymbolCellData::new(sym, special)))
    }

    pub fn data(&self) -> &mut SymbolCellData {
        self.0.get_mut()
    }
}
