use std::{marker::PhantomData, sync::Arc};
use std::sync::{LazyLock, RwLock};

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

use dashmap::DashMap;
use lasso::{Capacity, Key, Spur, ThreadedRodeo};
use proc_macros::{defun, Trace};

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
    map: DashMap<Spur, SymbolCell>,
    string_interner: lasso::ThreadedRodeo,
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
        let string_interner = ThreadedRodeo::with_capacity(Capacity::for_strings(size));
        Self { map, string_interner }
    }

    pub fn get(&self, name: &str) -> Option<Symbol> {
        let key = self.string_interner.get(name)?;
        Some(Symbol::new(key))
    }

    pub fn map(&self) -> &DashMap<Spur, SymbolCell>  {
        &self.map
    }

    pub fn intern(&self, name: &str) -> Symbol {
        let key = self.string_interner.get_or_intern(name);
        // Ensure the symbol cell exists in the map
        if !self.map.contains_key(&key) {
            let special = name.starts_with(':');
            self.map.insert(key, SymbolCell::new(key, special));
        }
        Symbol::new(key)
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

    pub fn from_string(ident: &str) -> Self {
        INTERNED_SYMBOLS.intern(ident)
    }

    pub fn name(&self) -> &str {
        INTERNED_SYMBOLS.string_interner.resolve(&self.name)
    }

    // TODO
    pub(crate) fn get(&self) -> Option<dashmap::mapref::one::Ref<'_, Spur, SymbolCell>> {
        // We need to find the cell by the spur key
        INTERNED_SYMBOLS.map.get(&self.name)

        // Find the index where the key matches
        // Since IndexMap uses usize indices, we need to find the position
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

    pub fn data(&self) -> &SymbolCellData {
        &self.0.get()
    }
}

#[defun]
fn test_name(a: Value, b: Value) -> Value {
    todo!()
}
