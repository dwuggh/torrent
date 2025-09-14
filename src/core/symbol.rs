//! Symbol management system for the Lisp interpreter.
//!
//! This module provides a comprehensive symbol system with:
//! - Symbol interning for memory efficiency
//! - Symbol cells for storing function and value bindings
//! - Thread-safe symbol map with garbage collection support
//! - Special symbol handling (keywords starting with ':')

use indexmap::IndexMap;
use proc_macros::Trace;

use crate::core::error::{RuntimeError, RuntimeResult};
use crate::core::ident::Ident;
use crate::core::object::{nil, LispType, Object};
use crate::core::tagged_ptr::{shifting_tag, shifting_untag, Tagged, TaggedObj};
use crate::gc::{Gc, Trace};

// =============================================================================
// Type Aliases and Constants
// =============================================================================

/// Internal map type for symbol storage
type Map = IndexMap<Ident, SymbolCell, rustc_hash::FxBuildHasher>;

/// Maximum index value used to indicate ident-only symbols
const IDENT_ONLY_INDEX: u32 = 0xFFFFFFFF;

// =============================================================================
// Core Symbol Types
// =============================================================================

/// A symbol in the Lisp system.
///
/// Symbols are represented as packed 64-bit values containing:
/// - Upper 32 bits: index in the symbol table (or IDENT_ONLY_INDEX)
/// - Lower 32 bits: identifier key
///
/// # Examples
///
/// ```rust
/// let sym = Symbol::from("hello");
/// println!("Symbol name: {}", sym.name());
/// ```
#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Symbol(pub u64);

/// Tagged wrapper for Symbol used in the object system.
///
/// This provides type safety when working with symbols in the tagged
/// pointer system.
#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct LispSymbol(pub Symbol);

/// Unpacked representation of a symbol for easier manipulation.
///
/// This separates the index and identifier components for direct access.
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct UnpackedLispSymbol {
    /// Index in the symbol table (IDENT_ONLY_INDEX if not interned)
    pub index: u32,
    /// The identifier key
    pub ident: Ident,
}

// =============================================================================
// Symbol Storage and Management
// =============================================================================

/// Thread-safe symbol table with garbage collection support.
///
/// The SymbolMap manages symbol interning and provides access to symbol cells.
/// It uses an IndexMap for O(1) access by both key and index.
///
/// # Examples
///
/// ```rust
/// let map = SymbolMap::default();
/// let sym = map.intern(Ident::from("test"), false);
/// assert_eq!(sym.0.name(), "test");
/// ```
#[derive(Debug, Trace)]
pub struct SymbolMap {
    map: Gc<SymbolMapInner>,
}

/// Internal storage for the symbol map.
#[derive(Debug, Default)]
struct SymbolMapInner(Map);

/// Container for symbol metadata and bindings.
///
/// Each symbol has an associated cell that stores:
/// - Function binding
/// - Value binding  
/// - Special/keyword status
/// - Interning status
#[derive(Debug, Trace, Clone)]
pub struct SymbolCell(pub Gc<SymbolCellData>);

/// Data stored in each symbol cell.
#[derive(Debug, Trace, Clone)]
pub struct SymbolCellData {
    /// The symbol this cell represents
    #[no_trace]
    pub name: Symbol,
    /// Whether this symbol is interned
    #[no_trace]
    pub interned: bool,
    /// Whether this is a special form or keyword
    #[no_trace]
    pub special: bool,
    /// Function binding (nil if unbound)
    pub func: Object,
    /// Value binding (nil if unbound)
    pub value: Object,
}

/// Collection of commonly used special symbols.
pub struct SpecialSymbols {
    pub nil: Symbol,
    pub t: Symbol,
}

// =============================================================================
// Symbol Implementation
// =============================================================================

impl Symbol {
    /// Create a new symbol with the given index and identifier.
    ///
    /// # Arguments
    /// * `index` - Position in the symbol table
    /// * `ident` - The identifier key
    ///
    /// # Examples
    ///
    /// ```rust
    /// let ident = Ident::from("test");
    /// let sym = Symbol::new(0, ident);
    /// ```
    pub fn new(index: usize, ident: Ident) -> Self {
        UnpackedLispSymbol::new(index, ident).into()
    }

    /// Create a symbol that is only identified by its identifier.
    ///
    /// These symbols are not interned and have no fixed index.
    ///
    /// # Arguments
    /// * `ident` - The identifier key
    pub fn new_ident(ident: Ident) -> Self {
        UnpackedLispSymbol::new_ident(ident).into()
    }

    /// Convert this symbol to a tagged Object.
    ///
    /// # Returns
    /// An Object containing this symbol with proper tagging.
    pub fn tag(self) -> Object {
        let symbol = LispSymbol::from(self);
        symbol.tag()
    }

    /// Get the string name of this symbol.
    ///
    /// # Returns
    /// A string slice containing the symbol's name.
    ///
    /// # Examples
    ///
    /// ```rust
    /// let sym = Symbol::from("hello");
    /// assert_eq!(sym.name(), "hello");
    /// ```
    pub fn name<'a>(&self) -> &'a str {
        self.ident().text()
    }

    /// Unpack this symbol into its components.
    ///
    /// # Returns
    /// An UnpackedLispSymbol with separate index and ident fields.
    pub fn unpack(&self) -> UnpackedLispSymbol {
        UnpackedLispSymbol::from(*self)
    }

    /// Get the identifier component of this symbol.
    ///
    /// # Returns
    /// The Ident used to create this symbol.
    pub fn ident(&self) -> Ident {
        let unpacked = self.unpack();
        unpacked.ident
    }

    /// Get the index of this symbol in the symbol table.
    ///
    /// # Returns
    /// Some(index) if the symbol is interned, None if it's ident-only.
    pub fn index(&self) -> Option<usize> {
        let unpacked = self.unpack();
        if unpacked.is_ident_only() {
            None
        } else {
            Some(unpacked.index as usize)
        }
    }
}

// =============================================================================
// UnpackedLispSymbol Implementation
// =============================================================================

impl UnpackedLispSymbol {
    /// Create a new unpacked symbol with index and identifier.
    fn new(index: usize, ident: Ident) -> Self {
        Self {
            index: index as u32,
            ident,
        }
    }

    /// Create an ident-only unpacked symbol.
    fn new_ident(ident: Ident) -> Self {
        Self {
            index: IDENT_ONLY_INDEX,
            ident,
        }
    }

    /// Create from raw u64 representation.
    ///
    /// The format is: [32-bit index][32-bit ident]
    pub fn from_raw_unpacked(value: u64) -> UnpackedLispSymbol {
        let index = (value >> 32) as u32;
        let ident = Ident((value & 0xFFFFFFFF) as u32);
        UnpackedLispSymbol { index, ident }
    }

    /// Convert to raw u64 representation.
    ///
    /// The format is: [32-bit index][32-bit ident]
    pub fn to_raw_packed(&self) -> u64 {
        (self.index as u64) << 32 | (self.ident.0 as u64)
    }

    /// Check if this symbol is ident-only (not interned).
    ///
    /// # Returns
    /// true if the symbol has no fixed index.
    pub fn is_ident_only(&self) -> bool {
        self.index == IDENT_ONLY_INDEX
    }
}

// =============================================================================
// SymbolMap Implementation
// =============================================================================

impl SymbolMap {
    /// Create a new symbol map with the specified capacity.
    ///
    /// # Arguments
    /// * `size` - Initial capacity for the symbol table
    pub fn with_capacity(size: usize) -> Self {
        let map = Map::with_capacity_and_hasher(size, rustc_hash::FxBuildHasher::default());
        Self {
            map: Gc::new(SymbolMapInner(map)),
        }
    }

    /// Get a symbol cell and apply a function to it.
    ///
    /// # Arguments
    /// * `symbol` - The symbol to look up
    /// * `job` - Function to apply to the symbol cell
    ///
    /// # Returns
    /// Result of the function or an unbound symbol error.
    pub fn get_symbol_cell_with<F, T>(&self, symbol: Symbol, job: F) -> RuntimeResult<T>
    where
        F: FnOnce(&SymbolCell) -> RuntimeResult<T>,
    {
        self.get_symbol_cell(symbol)
            .ok_or(RuntimeError::unbound_symbol(symbol))
            .and_then(job)
    }

    /// Intern a symbol with the given identifier.
    ///
    /// If the symbol already exists, returns the existing one.
    /// Otherwise, creates a new symbol and adds it to the table.
    ///
    /// # Arguments
    /// * `ident` - The identifier to intern
    /// * `special` - Whether this is a special form or keyword
    ///
    /// # Returns
    /// The interned LispSymbol.
    pub fn intern(&self, ident: Ident, special: bool) -> LispSymbol {
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

    /// Get an existing symbol by identifier.
    ///
    /// # Arguments
    /// * `ident` - The identifier to look up
    ///
    /// # Returns
    /// Some(Symbol) if found, None otherwise.
    pub fn get(&self, ident: Ident) -> Option<Symbol> {
        self.map()
            .get_index_of(&ident)
            .map(|index| Symbol::new(index, ident))
    }

    /// Get the symbol cell for a given symbol.
    ///
    /// # Arguments
    /// * `symbol` - The symbol to look up
    ///
    /// # Returns
    /// Some(SymbolCell) if found, None otherwise.
    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<&SymbolCell> {
        match symbol.index() {
            Some(index) => self.map().get_index(index).map(|(_, cell)| cell),
            None => self.map().get(&symbol.ident()),
        }
    }

    /// Get or initialize a symbol cell.
    ///
    /// If the symbol doesn't exist, it will be interned automatically.
    /// Keywords (starting with ':') are marked as special.
    ///
    /// # Arguments
    /// * `symbol` - The symbol to get or create
    ///
    /// # Returns
    /// Reference to the symbol cell.
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

    /// Get immutable reference to the internal map.
    #[inline]
    fn map(&self) -> &Map {
        &self.map.get().0
    }

    /// Get mutable reference to the internal map.
    #[inline]
    fn map_mut(&self) -> &mut Map {
        &mut self.map.get_mut().0
    }
}

// =============================================================================
// SymbolCell Implementation
// =============================================================================

impl SymbolCell {
    /// Create a new symbol cell.
    ///
    /// # Arguments
    /// * `sym` - The symbol this cell represents
    /// * `special` - Whether this is a special form or keyword
    pub fn new(sym: Symbol, special: bool) -> Self {
        SymbolCell(Gc::new(SymbolCellData::new(sym, special)))
    }

    /// Get mutable access to the cell data.
    ///
    /// # Returns
    /// Mutable reference to the SymbolCellData.
    pub fn data(&self) -> &mut SymbolCellData {
        self.0.get_mut()
    }

    /// Get the symbol this cell represents.
    ///
    /// # Returns
    /// The LispSymbol for this cell.
    pub fn symbol(&self) -> LispSymbol {
        LispSymbol(self.data().name)
    }
}

impl SymbolCellData {
    /// Create new symbol cell data.
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

// =============================================================================
// Trait Implementations
// =============================================================================

impl Tagged for LispSymbol {
    const TAG: LispType = LispType::Symbol;
    type Data<'a> = Symbol;
    type DataMut<'a> = Symbol;

    unsafe fn to_raw(&self) -> u64 {
        shifting_tag(self.0 .0 as u64, Self::TAG)
    }

    unsafe fn from_raw(raw: u64) -> Self {
        let val = shifting_untag(raw);
        std::mem::transmute(val)
    }

    unsafe fn cast<'a>(val: u64) -> Self::Data<'a> {
        Self::from_raw(val).0
    }

    unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a> {
        Self::from_raw(val).0
    }
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

// =============================================================================
// Conversion Implementations
// =============================================================================

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

impl From<&str> for LispSymbol {
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

// =============================================================================
// Display and Debug Implementations
// =============================================================================

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Symbol")
            .field("name", &self.name())
            .field("index", &self.index())
            .finish()
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name().fmt(f)
    }
}

// =============================================================================
// Default Implementations
// =============================================================================

impl Default for SymbolMap {
    fn default() -> Self {
        Self::with_capacity(128)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_creation() {
        let sym = Symbol::from("test");
        assert_eq!(sym.name(), "test");
        assert!(sym.index().is_none()); // Not interned yet
    }

    #[test]
    fn test_symbol_interning() {
        let map = SymbolMap::default();
        let sym1 = map.intern(Ident::from("test"), false);
        let sym2 = map.intern(Ident::from("test"), false);

        // Should be the same symbol
        assert_eq!(sym1.0, sym2.0);
        assert_eq!(sym1.0.name(), "test");
    }

    #[test]
    fn test_special_symbols() {
        let map = SymbolMap::default();
        let keyword = map.intern(Ident::from(":keyword"), true);
        let cell = map.get_symbol_cell(keyword.0).unwrap();
        assert!(cell.data().special);
    }

    #[test]
    fn test_symbol_packing() {
        let ident = Ident::from("test");
        let unpacked = UnpackedLispSymbol::new(42, ident);
        let packed = unpacked.to_raw_packed();
        let unpacked2 = UnpackedLispSymbol::from_raw_unpacked(packed);

        assert_eq!(unpacked.index, unpacked2.index);
        assert_eq!(unpacked.ident, unpacked2.ident);
    }

    #[test]
    fn test_ident_only_symbol() {
        let ident = Ident::from("test");
        let unpacked = UnpackedLispSymbol::new_ident(ident);
        assert!(unpacked.is_ident_only());

        let symbol = Symbol::from(unpacked);
        assert!(symbol.index().is_none());
    }

    #[test]
    fn test_symbol_cell_operations() {
        let sym = Symbol::from("test");
        let cell = SymbolCell::new(sym, false);

        assert_eq!(cell.symbol().0, sym);
        assert!(!cell.data().special);
        assert!(cell.data().interned);
    }

    #[test]
    fn test_tagged_symbol() {
        let sym = Symbol::from("test");
        let lisp_sym = LispSymbol(sym);

        let raw = unsafe { lisp_sym.to_raw() };
        let restored = unsafe { LispSymbol::from_raw(raw) };

        assert_eq!(lisp_sym.0.name(), restored.0.name());
    }

    #[test]
    fn test_symbol_map_get_or_init() {
        let map = SymbolMap::default();
        let sym = Symbol::from(":keyword");

        let cell = map.get_or_init_symbol(sym);
        assert!(cell.data().special); // Should detect ':' prefix
    }
}
