use std::{marker::PhantomData, sync::Arc};
use std::sync::{LazyLock, RwLock};

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

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

use indexmap::IndexMap;

#[derive(Debug)]
pub struct SymbolMap {
    inner: RwLock<IndexMap<Spur, SymbolCell>>,
    string_interner: lasso::ThreadedRodeo,
}

unsafe impl Trace for SymbolMap {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        for val in self.read().values() {
            val.trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for val in self.write().values_mut() {
            val.finalize();
        }
    }
}

impl SymbolMap {
    pub fn with_capacity(size: usize) -> Self {
        let map = IndexMap::with_capacity(size);
        let string_interner = ThreadedRodeo::with_capacity(Capacity::for_strings(size));
        Self { inner: RwLock::new(map), string_interner }
    }

    pub fn read(&self) -> std::sync::RwLockReadGuard<'_, IndexMap<Spur, SymbolCell>> {
        self.inner.read().unwrap()
    }
    pub fn write(&self) -> std::sync::RwLockWriteGuard<'_, IndexMap<Spur, SymbolCell>> {
        self.inner.write().unwrap()
    }

    pub fn get(&self, name: &str) -> Option<Symbol> {
        let key = self.string_interner.get(name)?;
        Some(Symbol::new(key))
    }

    pub fn intern(&self, name: &str) -> Symbol {
        let key = self.string_interner.get_or_intern(name);
        let mut map = self.write();
        // Ensure the symbol cell exists in the map
        if !map.contains_key(&key) {
            let special = name.starts_with(':');
            map.insert(key, SymbolCell::new(key, special));
        }
        Symbol::new(key)
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Symbol {
    #[no_trace]
    pub name: Spur,
    phantom: PhantomData<SymbolCell>,
}

impl Trace for Symbol {
    unsafe fn trace(&self, _visitor: crate::gc::Visitor) {
        // No-op: Symbol only contains interned name which doesn't need tracing
    }

    unsafe fn finalize(&mut self) {
        // No-op
    }
}

impl Symbol {
    pub fn new(name: Spur) -> Self {
        Self {
            name,
            phantom: PhantomData,
        }
    }

    pub fn from_string(ident: &str) -> Self {
        INTERNED_SYMBOLS.intern(ident)
    }

    pub fn name(&self) -> &str {
        INTERNED_SYMBOLS.string_interner.resolve(&self.name)
    }

    pub(crate) fn get<'a>(self) -> Option<SymbolCell>  {
        // We need to find the cell by the spur key
        let map = INTERNED_SYMBOLS.read();
        // Find the index where the key matches
        // Since IndexMap uses usize indices, we need to find the position
        for (i, (k, v)) in map.iter().enumerate() {
            if k == &self.name {
                return Some(v.clone());
            }
        }
        None
    }
}

const KEY_MASK: u64 = u64::MAX;

impl TaggedPtr for Symbol {
    const TAG: LispType = LispType::Symbol;

    unsafe fn cast(val: u64) -> Self {
        let key = (val & KEY_MASK) as usize;
        Symbol {
            name: Spur::try_from_usize(key).unwrap(),
            phantom: PhantomData,
        }
    }

    unsafe fn get_untagged_data(self) -> u64 {
        self.name.into_usize() as u64
    }
}

#[derive(Debug, Trace, Clone)]
pub struct SymbolCellData {
    #[no_trace]
    pub name: Spur,
    pub interned: bool,
    pub special: bool,
    pub func: Option<Value>,
    pub value: Option<Value>,
}

#[derive(Debug, Trace, Clone)]
pub struct SymbolCell(SymbolCellData);

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
        SymbolCell(SymbolCellData::new(name, special))
    }

    pub fn data(&self) -> &SymbolCellData {
        &self.0
    }
}
