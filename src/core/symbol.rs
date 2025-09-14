use indexmap::IndexMap;
use proc_macros::Trace;

use crate::core::error::{RuntimeError, RuntimeResult};
use crate::core::ident::Ident;
use crate::core::object::nil;
use crate::core::tagged_ptr::{shifting_tag, shifting_untag, TaggedObj};
use crate::gc::Trace;
use crate::{core::object::Object, core::Tagged, gc::Gc};

use super::object::LispType;

// TODO maybe change this to String?
type Map = IndexMap<Ident, SymbolCell, rustc_hash::FxBuildHasher>;

#[derive(Debug, Trace)]
pub struct SymbolMap {
    map: Gc<SymbolMapInner>,
}

pub struct SpecialSymbols {
    nil: Symbol,
    t: Symbol,
}

#[derive(Debug, Default)]
struct SymbolMapInner(Map);

#[derive(Debug, Trace, Clone)]
pub struct SymbolCell(pub Gc<SymbolCellData>);

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

#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct LispSymbol(pub Symbol);

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Symbol(pub u64);

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct UnpackedLispSymbol {
    pub index: u32,
    pub ident: Ident,
}

unsafe impl Trace for SymbolMapInner {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        for (_, cell) in self.0.iter() {
            cell.trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for (_, cell) in self.0.iter_mut() {
            cell.finalize();
        }
    }
}

impl SymbolMap {
    pub fn with_capacity(size: usize) -> Self {
        let map = Map::with_capacity_and_hasher(size, rustc_hash::FxBuildHasher::default());
        Self {
            map: Gc::new(SymbolMapInner(map)),
        }
    }

    pub fn get_symbol_cell_with<F, T>(&self, symbol: Symbol, job: F) -> RuntimeResult<T>
    where
        F: FnOnce(&SymbolCell) -> RuntimeResult<T>,
    {
        self.get_symbol_cell(symbol)
            .ok_or(RuntimeError::unbound_symbol(symbol))
            .and_then(job)
    }

    pub fn intern(&self, ident: Ident, special: bool) -> LispSymbol {
        // Ensure the symbol cell exists in the map
        let map = self.map_mut();
        match map.entry(ident) {
            indexmap::map::Entry::Occupied(occupied_entry) => {
                let index = occupied_entry.index();
                LispSymbol::from(UnpackedLispSymbol::new(index, ident))
            }
            indexmap::map::Entry::Vacant(vacant_entry) => {
                let index = vacant_entry.index();
                let unpacked = UnpackedLispSymbol::new(index, ident);
                let symbol = Symbol::from(unpacked);
                vacant_entry.insert(SymbolCell::new(symbol, special));
                LispSymbol(symbol)
            }
        }
    }

    pub fn get(&self, ident: Ident) -> Option<Symbol> {
        self.map().get_index_of(&ident).map(|index| Symbol::new(index, ident))
    }

    /// Get SymbolCellData by index
    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<&SymbolCell> {
        match symbol.index() {
            Some(index) => self.map().get_index(index).map(|(_, cell)| cell),
            None => {
                self.map().get(&symbol.ident())
            }
        }
    }

    pub fn get_or_init_symbol(&self, symbol: Symbol) -> &SymbolCell {
        match symbol.index() {
            Some(index) => self.map().get_index(index).map(|(_, cell)| cell).unwrap(),
            None => {
                let text = symbol.name();
                let special = text.starts_with(':');
                let sym = self.intern(symbol.ident(), special);
                self.get_symbol_cell(sym.0).unwrap()
            }
        }
    }

    #[inline]
    fn map(&self) -> &Map {
        &self.map.get().0
    }

    #[inline]
    fn map_mut(&self) -> &mut Map {
        &mut self.map.get_mut().0
    }
}

impl Symbol {
    pub fn new(index: usize, ident: Ident) -> Self {
        UnpackedLispSymbol::new(index, ident).into()
    }

    pub fn new_ident(ident: Ident) -> Self {
        UnpackedLispSymbol::new_ident(ident).into()
    }

    pub fn tag(self) -> Object {
        let symbol = LispSymbol::from(self);
        symbol.tag()
    }

    pub fn name<'a>(&self) -> &'a str {
        self.ident().text()
    }

    pub fn unpack(&self) -> UnpackedLispSymbol {
        UnpackedLispSymbol::from(*self)
    }

    pub fn ident(&self) -> Ident {
        let unpacked = self.unpack();
        unpacked.ident
    }
    pub fn index(&self) -> Option<usize> {
        let unpacked = self.unpack();
        if unpacked.is_ident_only() {
            None
        } else {
            Some(unpacked.index as usize)
        }
    }
}

impl UnpackedLispSymbol {
    fn new(index: usize, ident: Ident) -> Self {
        Self {
            index: index as u32,
            ident,
        }
    }

    fn new_ident(ident: Ident) -> Self {
        let index = 0xFFFFFFFF;
        Self {
            index,
            ident,
        }
    }

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

impl From<Symbol> for LispSymbol {
    fn from(value: Symbol) -> Self {
        LispSymbol(value)
    }
}

impl From<Symbol> for UnpackedLispSymbol {
    fn from(value: Symbol) -> Self {
        Self::from_raw_unpacked(value.0)
    }
}

impl From<UnpackedLispSymbol> for Symbol {
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
        self.name().fmt(f)
    }
}

impl From<&str> for LispSymbol{
    fn from(data: &str) -> Self {
        Self(Symbol::from(data))
    }
}

impl From<UnpackedLispSymbol> for LispSymbol {
    fn from(data: UnpackedLispSymbol) -> Self {
        Self::from(Symbol::from(data))
    }
}

impl From<Ident> for Symbol {
    fn from(ident: Ident) -> Self {
        Symbol::new_ident(ident)
    }
}

impl From<&Ident> for Symbol {
    fn from(ident: &Ident) -> Self {
        Symbol::new_ident(*ident)
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        let ident = Ident::from(value);
        Symbol::new_ident(ident)
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        let ident = Ident::from(value);
        Symbol::new_ident(ident)
    }
}

impl Default for SymbolMap {
    fn default() -> Self {
        Self::with_capacity(128)
    }
}

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

    pub fn symbol(&self) -> LispSymbol {
        LispSymbol(self.data().name)
    }

}

impl Tagged for LispSymbol {
    const TAG: LispType = LispType::Symbol;
    type Data<'a> = Symbol;
    type DataMut<'a> = Symbol;

    unsafe fn to_raw(&self) -> u64 {
        shifting_tag(self.0.0, Self::TAG)
    }

    unsafe fn from_raw(raw: u64) -> Self {
        let val = shifting_untag(raw);
        Self(Symbol(val))
    }

    unsafe fn cast<'a>(val: u64) -> Self::Data<'a> {
        Self::from_raw(val).0
    }

    unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a> {
        Self::from_raw(val).0
    }
}
