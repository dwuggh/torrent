use std::{marker::PhantomData, sync::Arc};
use std::sync::{LazyLock, RwLock};

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

use dashmap::DashMap;
use lasso::{Capacity, Key, Spur, ThreadedRodeo};
use proc_macros::Trace;

use crate::gc::Trace;
use crate::{
    core::value::{TaggedPtr, Value},
    gc::Gc,
};

use super::value::LispType;

pub static INTERNED_SYMBOLS: LazyLock<SymbolMap> = LazyLock::new(|| {
    SymbolMap::with_capacity(100)
});

#[derive(Debug)]
pub struct SymbolMap {
    map: DashMap<Spur, SymbolCell>,
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

    pub fn map(&self) -> &DashMap<Spur, SymbolCell>  {
        &self.map
    }

    pub fn intern(&self, symbol: Symbol, special: bool) -> Symbol {
        // Ensure the symbol cell exists in the map
        if !self.map.contains_key(&symbol.name) {
            self.map.insert(symbol.name, SymbolCell::new(symbol.name, special));
        }
        symbol
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Trace)]
pub struct Symbol {
    #[no_trace]
    pub name: Spur,
    phantom: PhantomData<SymbolCell>,
}

impl Symbol {
    pub fn new(name: Spur) -> Self {
        Self {
            name,
            phantom: PhantomData,
        }
    }

    pub fn from_string(ident: &str, interner: &mut lasso::ThreadedRodeo) -> Self {
        let spur = interner.get_or_intern(ident);
        let symbol = Symbol::new(spur);
        let special = ident.starts_with(':');
        INTERNED_SYMBOLS.intern(symbol, special);
        symbol
    }

    pub fn name<'a>(&self, interner: &'a lasso::ThreadedRodeo) -> &'a str {
        interner.resolve(&self.name)
    }

    // TODO
    pub(crate) fn get(&self) -> Option<dashmap::mapref::one::Ref<'_, Spur, SymbolCell>> {
        INTERNED_SYMBOLS.map.get(&self.name)
    }

    pub fn get_or_init(&self) -> dashmap::mapref::one::RefMut<'_, Spur, SymbolCell> {
        INTERNED_SYMBOLS.map.get_mut(&self.name).unwrap()
    }
}

impl TaggedPtr for Symbol {
    const TAG: LispType = LispType::Symbol;

    unsafe fn cast(val: u64) -> Self {
        let key = val as usize;
        Symbol {
            name: Spur::try_from_usize(key).unwrap(),
            phantom: PhantomData,
        }
    }

    unsafe fn get_untagged_data(self) -> u64 {
        self.name.into_usize() as u64
    }
}

#[derive(Debug, Trace, Clone, Copy)]
pub struct SymbolCellData {
    #[no_trace]
    pub name: Spur,
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
    fn new(name: Spur, special: bool) -> Self {
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
    pub fn new(name: Spur, special: bool) -> Self {
        SymbolCell(Gc::new(SymbolCellData::new(name, special)))
    }

    pub fn data(&self) -> &mut SymbolCellData {
        self.0.get_mut()
    }
}
