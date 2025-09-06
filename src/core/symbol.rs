use std::sync::{LazyLock, RwLock};
use std::{marker::PhantomData, sync::Arc};

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

use dashmap::DashMap;
use lasso::{Key, Spur};
use proc_macros::Trace;

use crate::core::ident::Ident;
use crate::gc::Trace;
use crate::{
    core::value::{TaggedPtr, Value},
    gc::Gc,
};

use super::value::LispType;

pub static INTERNED_SYMBOLS: LazyLock<SymbolMap> = LazyLock::new(|| SymbolMap::with_capacity(100));

#[derive(Debug)]
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
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Trace)]
pub struct Symbol {
    #[no_trace]
    pub name: Ident,
    phantom: PhantomData<SymbolCell>,
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

impl Symbol {
    pub fn new(name: Ident) -> Self {
        Self {
            name,
            phantom: PhantomData,
        }
    }

    pub fn from_string(ident: &str) -> Self {
        let special = ident.starts_with(':');
        let ident = ident.into();
        let symbol = Symbol::new(ident);
        INTERNED_SYMBOLS.intern(symbol, special);
        symbol
    }

    pub fn name<'a>(&self) -> &'a str {
        self.name.text()
    }

    // TODO
    pub(crate) fn get(&self) -> Option<dashmap::mapref::one::Ref<'_, Symbol, SymbolCell>> {
        INTERNED_SYMBOLS.map.get(&self)
    }

    pub fn get_or_init(&self) -> dashmap::mapref::one::RefMut<'_, Symbol, SymbolCell> {
        INTERNED_SYMBOLS.map.get_mut(&self).unwrap()
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
    pub func: Option<Value>,
    pub value: Option<Value>,
}

#[derive(Debug, Trace, Clone)]
pub struct SymbolCell(pub Gc<SymbolCellData>);

impl SymbolCellData {
    fn new(name: Symbol, special: bool) -> Self {
        Self {
            name,
            interned: true,
            func: None,
            value: None,
            special,
        }
    }
}

impl SymbolCell {
    pub fn new(name: Symbol, special: bool) -> Self {
        SymbolCell(Gc::new(SymbolCellData::new(name, special)))
    }

    pub fn data(&self) -> &mut SymbolCellData {
        self.0.get_mut()
    }
}
