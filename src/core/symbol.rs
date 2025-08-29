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
        let key = self.string_interner.get_or_intern(name);
        let map = self.read();
        let offset = map.get_index_of(&key)?;
        Some(Symbol::new(offset, key))
    }

    pub fn intern<'ob>(&self, name: &str) -> Symbol {
        let key = self.string_interner.get_or_intern(name);
        let mut map = self.write();
        if let Some(index) = map.get_index_of(&key) {
            Symbol::new(index, key)
        } else {
            let special = name.starts_with(':');
            map.insert(key, SymbolCell::new(key, special));
            Symbol::new(map.get_index_of(&key).unwrap(), key)
        }
    }
}

#[repr(C)]
#[derive(Clone, Trace, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Symbol {
    #[no_trace]
    pub index: u32,
    #[no_trace]
    pub name: Spur,
    phantom: PhantomData<SymbolCell>,
}

impl Symbol {
    pub fn new(index: usize, name: Spur) -> Self {
        Self {
            index: index as u32,
            name,
            phantom: PhantomData,
        }
    }

    pub fn from_string(ident: &str) -> Self {
        INTERNED_SYMBOLS.intern(ident)
    }

    pub fn index(self) -> usize {
        self.index as usize
    }

    pub fn name(&self) -> Option<&str> {
        Some(INTERNED_SYMBOLS.string_interner.resolve(&self.name))
    }

    pub(crate) fn get<'a>(self) -> Option<SymbolCell>  {
        INTERNED_SYMBOLS.read().get_index(self.index as usize).map(|(k, v)| {
            let a = v.clone();
            a
        })
    }
}


const KEY_MASK: u64 = (1 << 32) - 1;

impl TaggedPtr for Symbol {
    const TAG: LispType = LispType::Symbol;

    unsafe fn cast(val: u64) -> Self {
        let key = (val & KEY_MASK) as usize;
        let index = ((val >> 32) & KEY_MASK) as u32;
        Self {
            index,
            name: Spur::try_from_usize(key).unwrap(),
            phantom: PhantomData,
        }
    }

    unsafe fn get_untagged_data(self) -> u64 {
        ((self.index as u64) << 32) | (self.name.into_usize() as u64)
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
