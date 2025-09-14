use std::marker::PhantomData;

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

use indexmap::IndexMap;
use proc_macros::Trace;
use scc::HashIndex;

use crate::core::error::{RuntimeError, RuntimeResult};
use crate::core::ident::Ident;
use crate::core::object::nil;
use crate::core::tagged_ptr::{shifting_tag, shifting_untag};
use crate::gc::Trace;
use crate::{core::object::Object, core::Tagged, gc::Gc};

use super::object::LispType;

type Map = IndexMap<Symbol, SymbolCell, rustc_hash::FxBuildHasher>;

#[derive(Debug, Default)]
pub struct SymbolMap {
    map: Map,
}

unsafe impl Trace for SymbolMap {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        self.map.iter().map(|(_,cell)| {
            cell.trace(visitor);
            true
        });
    }

    unsafe fn finalize(&mut self) {
        for (_, cell) in self.map.iter_mut() {
            cell.finalize();
        }
    }
}

impl SymbolMap {
    pub fn with_capacity(size: usize) -> Self {
        let map = Map::with_capacity_and_hasher(size, rustc_hash::FxBuildHasher::default());
        Self { map }
    }

    pub fn map(&self) -> &Map {
        &self.map
    }

    pub fn get_symbol_cell_with<F, T>(&self, symbol: Symbol, job: F) -> RuntimeResult<T>
    where
        F: FnOnce(&SymbolCell) -> RuntimeResult<T>,
    {
        self.map
            .peek_with(&symbol, |_k, v| job(v))
            .ok_or(RuntimeError::unbound_symbol(symbol))
            .flatten()
    }

    pub fn intern(&self, symbol: Symbol, special: bool) -> Symbol {
        // Ensure the symbol cell exists in the map
        if !self.map.contains(&symbol) {
            self.map
                .insert_sync(symbol, SymbolCell::new(symbol, special))
                .unwrap();
        }
        symbol
    }
}

#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Trace)]
pub struct Symbol {
    #[no_trace]
    pub data: LispSymbol,
    phantom: PhantomData<SymbolCell>,
}

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct LispSymbol(pub u64);

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct UnpackedLispSymbol {
    pub index: u32,
    pub ident: Ident,
}

impl UnpackedLispSymbol {
    /// Creates an unpacked symbol from raw u64 where first 32 bits are index, last 32 bits are ident
    pub fn from_raw_unpacked(value: u64) -> UnpackedLispSymbol {
        let index = (value >> 32) as u32;
        let ident = Ident((value & 0xFFFFFFFF) as u32);
        UnpackedLispSymbol { index, ident }
    }

    /// Converts to raw u64 representation where first 32 bits are index, last 32 bits are ident
    pub fn to_raw_packed(&self) -> u64 {
        (self.index as u64) << 32 | (self.ident.0 as u64)
    }

    /// Returns true if this symbol is only determined by its ident (index is u32::MAX)
    pub fn is_ident_only(&self) -> bool {
        self.index == u32::MAX
    }
}

impl From<LispSymbol> for UnpackedLispSymbol {
    fn from(value: LispSymbol) -> Self {
        Self::from_raw_unpacked(value.0)
    }
}

impl From<UnpackedLispSymbol> for LispSymbol {
    fn from(value: UnpackedLispSymbol) -> Self {
        Self(value.to_raw_packed())
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Symbol")
            .field("name", &self.name())
            .finish()
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl Symbol {
    pub fn new(name: Ident, symbol_map: &SymbolMap) -> Self {
        todo!()
    }

    pub fn name<'a>(&self) -> &'a str {
        self.name.text()
    }
}

impl Tagged for Symbol {
    const TAG: LispType = LispType::Symbol;
    type Data<'a> = LispSymbol;
    type DataMut<'a> = LispSymbol;
    unsafe fn to_raw(&self) -> u64 {
        shifting_tag(self.name.0, Self::TAG)
    }
    unsafe fn from_raw(raw: u64) -> Self {
        let val = shifting_untag(raw);
        std::mem::transmute(val)
    }
    unsafe fn cast<'a>(val: u64) -> Self::Data<'a> {
        Self::from_raw(val)
    }
    unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a> {
        Self::from_raw(val)
    }
}

impl AsRef<Symbol> for Symbol {
    fn as_ref(&self) -> &Symbol {
        self
    }
}

impl AsMut<Symbol> for Symbol {
    fn as_mut(&mut self) -> &mut Symbol {
        self
    }
}

#[derive(Debug, Trace, Clone)]
pub struct SymbolCellData {
    #[no_trace]
    pub name: Symbol,
    #[no_trace]
    pub interned: bool,
    #[no_trace]
    pub special: bool,
    pub func: Object,
    pub value: Object,
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
