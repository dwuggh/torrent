//! Symbol management system for the Lisp interpreter.
//!
//! This module provides a comprehensive symbol system with:
//! - Symbol interning for memory efficiency
//! - Symbol cells for storing function and value bindings
//! - Thread-safe symbol map with garbage collection support
//! - Special symbol handling (keywords starting with ':')

use crate::core::error::RuntimeResult;
use crate::core::ident::Ident;
use crate::core::object::{Object, nil, write_object_slot};
use crate::core::tag::{TAG_SYMBOL_MAP, Tag, TaggedPtrError, Untag};
use crate::gc::{HeaderedObject, Trace, Visitor};
use indexmap::IndexMap;

// =============================================================================
// Type Aliases and Constants
// =============================================================================

const SYMBOL_TAG: u64 = crate::gc::GcTag::SYMBOL as u64;
const SYMBOL_TAG_BITS: u32 = 3;
const SYMBOL_SEMANTICS_BITS: usize = 2;
const SYMBOL_SEMANTICS_MASK: usize = (1 << SYMBOL_SEMANTICS_BITS) - 1;
const SYMBOL_DATA_BITS: u32 = 59;
const SYMBOL_DATA_MASK: usize = (u64::MAX >> (64 - SYMBOL_DATA_BITS)) as usize;

// =============================================================================
// Core Symbol Types
// =============================================================================

/// A symbol in the Lisp system.
///
/// Symbols are represented as a 61-bit payload: 59 bits of data plus 2 bits of
/// compile-time semantics. The tagged object form appends the low 3-bit symbol
/// tag (`001`).
///
/// # Examples
///
/// ```rust
/// # use torrent::core::symbol::Symbol;
/// let sym = Symbol::from("hello");
/// println!("Symbol name: {}", sym.name());
/// ```
#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Symbol(pub usize);

#[repr(u8)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum SymbolSemantics {
    Global = 0,
    Argument = 1,
    Lexical = 2,
    Unresolved = 3,
}

/// Tagged wrapper for Symbol used in the object system.
///
/// This provides type safety when working with symbols in the tagged
/// pointer system.
#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct LispSymbol(pub Symbol);

// =============================================================================
// Symbol Storage and Management
// =============================================================================

/// Internal map type for symbol storage
type Map = IndexMap<Ident, SymbolCell, rustc_hash::FxBuildHasher>;

/// Thread-safe symbol table with garbage collection support.
///
/// The SymbolMap manages symbol interning and provides access to symbol cells.
/// It uses an IndexMap for O(1) access by both key and index.
///
/// # Examples
///
/// ```rust
/// # use torrent::core::{symbol::SymbolMap, ident::Ident};
/// let mut map = SymbolMap::default();
/// let sym = map.intern(Ident::from("test"), false);
/// assert_eq!(sym.0.index(), Some(0));
/// ```
#[derive(Debug)]
pub struct SymbolMap {
    map: Map,
}

/// Container for symbol metadata and bindings.
///
/// Each symbol has an associated cell that stores:
/// - Function binding
/// - Value binding  
/// - Special/keyword status
/// - Interning status
#[repr(C)]
#[derive(Debug, Clone)]
pub struct SymbolCell {
    /// The symbol this cell represents
    pub name: Symbol,
    /// Function binding (nil if unbound)
    pub func: Object,
    /// Value binding (nil if unbound)
    pub value: Object,
    /// Whether this symbol is interned
    pub interned: bool,
    /// Whether this is a special form or keyword
    pub special: bool,
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
    pub fn new(data: usize, semantics: SymbolSemantics) -> Self {
        assert!(
            data <= SYMBOL_DATA_MASK,
            "symbol data exceeds 59-bit immediate symbol encoding"
        );
        Self((data << SYMBOL_SEMANTICS_BITS) | semantics as usize)
    }

    pub fn new_global(index: usize) -> Self {
        Self::new(index, SymbolSemantics::Global)
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
    /// # use torrent::core::symbol::Symbol;
    /// let sym = Symbol::from("hello");
    /// assert_eq!(sym.name(), "hello");
    /// ```
    pub fn name<'a>(&self) -> &'a str {
        if self.semantics() == SymbolSemantics::Unresolved {
            self.ident().text()
        } else {
            "<indexed-symbol>"
        }
    }

    /// Get the identifier component of this symbol.
    ///
    /// # Returns
    /// The Ident used to create this symbol.
    pub fn ident(&self) -> Ident {
        if self.semantics() == SymbolSemantics::Unresolved {
            Ident::from_raw(self.data() as u32)
        } else {
            Ident::from_raw(0)
        }
    }

    pub fn index(&self) -> Option<usize> {
        if self.semantics() == SymbolSemantics::Unresolved {
            None
        } else {
            Some(self.data())
        }
    }

    pub fn data(&self) -> usize {
        self.0 >> SYMBOL_SEMANTICS_BITS
    }

    pub fn semantics(&self) -> SymbolSemantics {
        match self.0 & SYMBOL_SEMANTICS_MASK {
            0 => SymbolSemantics::Global,
            1 => SymbolSemantics::Argument,
            2 => SymbolSemantics::Lexical,
            3 => SymbolSemantics::Unresolved,
            _ => unreachable!(),
        }
    }

    /// Return true if this symbol has a fixed symbol-table index.
    pub fn is_indexed(&self) -> bool {
        self.index().is_some()
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
        Self { map }
    }

    // (removed: earlier inlined version of get_symbol_cell_with)

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
    pub fn intern(&mut self, ident: Ident, special: bool) -> LispSymbol {
        let map = &mut self.map;
        match map.entry(ident) {
            indexmap::map::Entry::Occupied(occupied_entry) => {
                let index = occupied_entry.index();
                LispSymbol::from(Symbol::new_global(index))
            }
            indexmap::map::Entry::Vacant(vacant_entry) => {
                let index = vacant_entry.index();
                let symbol = Symbol::new_global(index);
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
        self.map.get_index_of(&ident).map(Symbol::new_global)
    }

    /// Get a symbol cell and apply a function to it.
    /// Returns an error if the symbol is unbound.
    pub fn get_symbol_cell_with<F, T>(&self, symbol: Symbol, job: F) -> RuntimeResult<T>
    where
        F: FnOnce(&SymbolCell) -> RuntimeResult<T>,
    {
        let cell = self
            .get_symbol_cell(symbol)
            .ok_or(crate::core::error::RuntimeError::unbound_symbol(symbol))?;
        job(&cell)
    }

    /// Borrow the symbol cell by reference without cloning.
    pub fn get_symbol_cell_ref(&self, symbol: Symbol) -> Option<&SymbolCell> {
        match symbol.index() {
            Some(index) => self.map.get_index(index).map(|(_, cell)| cell),
            None => self.map.get(&symbol.ident()),
        }
    }

    /// Get the symbol cell for a given symbol.
    ///
    /// # Arguments
    /// * `symbol` - The symbol to look up
    ///
    /// # Returns
    /// Some(SymbolCell) if found, None otherwise.
    pub fn get_symbol_cell(&self, symbol: Symbol) -> Option<SymbolCell> {
        match symbol.index() {
            Some(index) => self.map.get_index(index).map(|(_, cell)| cell.clone()),
            None => self.map.get(&symbol.ident()).cloned(),
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
    pub fn get_or_init_symbol(&mut self, symbol: Symbol) -> SymbolCell {
        match symbol.index() {
            Some(index) => self
                .map
                .get_index(index)
                .map(|(_, cell)| cell.clone())
                .unwrap(),
            None => {
                let text = symbol.name();
                let special = text.starts_with(':');
                let sym = self.intern(symbol.ident(), special);
                self.get_symbol_cell(sym.0).unwrap()
            }
        }
    }

    /// Mutate the value cell of a symbol (initializing it if needed).
    pub fn set_value(&mut self, symbol: Symbol, value: Object) -> Symbol {
        let owner = Object::from_heap_ref(&*self);
        match symbol.index() {
            Some(index) => {
                if let Some((_, cell)) = self.map.get_index_mut(index) {
                    write_object_slot(owner, &mut cell.value, value);
                    cell.name
                } else {
                    symbol
                }
            }
            None => {
                let ident = symbol.ident();
                let special = symbol.name().starts_with(':');
                match self.map.entry(ident) {
                    indexmap::map::Entry::Occupied(mut occupied_entry) => {
                        let symbol = Symbol::new_global(occupied_entry.index());
                        let cell = occupied_entry.get_mut();
                        cell.name = symbol;
                        write_object_slot(owner, &mut cell.value, value);
                        symbol
                    }
                    indexmap::map::Entry::Vacant(vacant_entry) => {
                        let symbol = Symbol::new_global(vacant_entry.index());
                        let cell = vacant_entry.insert(SymbolCell::new(symbol, special));
                        write_object_slot(owner, &mut cell.value, value);
                        symbol
                    }
                }
            }
        }
    }

    /// Mutate the function cell of a symbol (initializing it if needed).
    pub fn set_func(&mut self, symbol: Symbol, func: Object) -> Symbol {
        let owner = Object::from_heap_ref(&*self);
        match symbol.index() {
            Some(index) => {
                if let Some((_, cell)) = self.map.get_index_mut(index) {
                    write_object_slot(owner, &mut cell.func, func);
                    cell.name
                } else {
                    symbol
                }
            }
            None => {
                let ident = symbol.ident();
                let special = symbol.name().starts_with(':');
                match self.map.entry(ident) {
                    indexmap::map::Entry::Occupied(mut occupied_entry) => {
                        let symbol = Symbol::new_global(occupied_entry.index());
                        let cell = occupied_entry.get_mut();
                        cell.name = symbol;
                        write_object_slot(owner, &mut cell.func, func);
                        symbol
                    }
                    indexmap::map::Entry::Vacant(vacant_entry) => {
                        let symbol = Symbol::new_global(vacant_entry.index());
                        let cell = vacant_entry.insert(SymbolCell::new(symbol, special));
                        write_object_slot(owner, &mut cell.func, func);
                        symbol
                    }
                }
            }
        }
    }

    // Accessors removed; `map` is owned.
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
        SymbolCell {
            name: sym,
            interned: true,
            special,
            func: nil(),
            value: nil(),
        }
    }

    /// Get the symbol this cell represents.
    ///
    /// # Returns
    /// The LispSymbol for this cell.
    pub fn symbol(&self) -> LispSymbol {
        LispSymbol(self.name)
    }
}

// =============================================================================
// Trait Implementations
// =============================================================================

impl Tag for LispSymbol {
    fn tag(self) -> Object {
        let symbol = self.0;
        let raw = ((symbol.0 as u64) << SYMBOL_TAG_BITS) | SYMBOL_TAG;
        Object::from_raw(raw)
    }
}

impl Untag for LispSymbol {
    fn untag(object: Object) -> Result<Self, TaggedPtrError> {
        if object.get_tag() == crate::gc::GcTag::SYMBOL {
            let payload = object.raw() >> SYMBOL_TAG_BITS;
            Ok(Self(Symbol(payload as usize)))
        } else {
            Err(TaggedPtrError::TypeMisMatch)
        }
    }
}

impl crate::gc::Tagged for SymbolMap {
    const TAG: u8 = TAG_SYMBOL_MAP;
}

unsafe impl HeaderedObject for SymbolMap {}

unsafe impl Trace for SymbolCell {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        unsafe { self.func.trace(visitor) };
        unsafe { self.value.trace(visitor) };
    }
}

unsafe impl Trace for SymbolMap {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        // Symbol cells hold global function/value bindings as tagged slots.
        for (_, cell) in self.map.iter() {
            unsafe { cell.trace(visitor) };
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

impl From<&str> for LispSymbol {
    fn from(data: &str) -> Self {
        Self(Symbol::from(data))
    }
}

impl From<Ident> for Symbol {
    fn from(ident: Ident) -> Self {
        Symbol::new(ident.0 as usize, SymbolSemantics::Unresolved)
    }
}

impl From<&Ident> for Symbol {
    fn from(ident: &Ident) -> Self {
        Symbol::new(ident.0 as usize, SymbolSemantics::Unresolved)
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        let ident = Ident::from(value);
        Symbol::from(ident)
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        let ident = Ident::from(value);
        Symbol::from(ident)
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
            .field("semantics", &self.semantics())
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
    use crate::core::error::RuntimeError;

    fn raw_symbol_tag(object: &Object) -> u64 {
        object.raw() & crate::gc::GcTag::MASK as u64
    }

    #[test]
    fn test_symbol_creation() {
        let sym = Symbol::from("test");
        assert_eq!(sym.name(), "test");
        assert!(sym.index().is_none()); // Not interned yet
        assert_eq!(sym.semantics(), SymbolSemantics::Unresolved);
    }

    #[test]
    fn test_symbol_interning() {
        let mut map = SymbolMap::default();
        let sym1 = map.intern(Ident::from("test"), false);
        let sym2 = map.intern(Ident::from("test"), false);

        // Should be the same symbol
        assert_eq!(sym1.0, sym2.0);
        assert_eq!(sym1.0.name(), "<indexed-symbol>");
        assert!(sym1.0.is_indexed());
    }

    #[test]
    fn test_special_symbols() {
        let mut map = SymbolMap::default();
        let keyword = map.intern(Ident::from(":keyword"), true);
        let cell = map.get_symbol_cell(keyword.0).unwrap();
        assert!(cell.special);
    }

    #[test]
    fn test_unresolved_symbol_layout() {
        let ident = Ident::from("test");
        let symbol = Symbol::from(ident);
        let raw = LispSymbol(symbol).tag();
        let restored = LispSymbol::untag(raw.clone()).unwrap().0;

        assert_eq!(raw_symbol_tag(&raw), SYMBOL_TAG);
        assert_eq!(symbol.data(), ident.0 as usize);
        assert_eq!(symbol.semantics(), SymbolSemantics::Unresolved);
        assert_eq!(restored, symbol);
        assert_eq!(restored.ident(), ident);
    }

    #[test]
    fn test_symbol_cell_operations() {
        let sym = Symbol::from("test");
        let cell = SymbolCell::new(sym, false);

        assert_eq!(cell.symbol().0, sym);
        assert!(!cell.special);
        assert!(cell.interned);
    }

    #[test]
    fn test_symbol_layout_round_trip_all_semantics() {
        for semantics in [
            SymbolSemantics::Global,
            SymbolSemantics::Argument,
            SymbolSemantics::Lexical,
            SymbolSemantics::Unresolved,
        ] {
            let symbol = Symbol::new(42, semantics);
            let raw = LispSymbol(symbol).tag();
            let restored = LispSymbol::untag(raw.clone()).unwrap().0;

            assert_eq!(raw_symbol_tag(&raw), SYMBOL_TAG);
            assert_eq!(raw.raw() >> SYMBOL_TAG_BITS, symbol.0 as u64);
            assert_eq!(restored, symbol);
            assert_eq!(restored.data(), 42);
            assert_eq!(restored.semantics(), semantics);
        }
    }

    #[test]
    fn test_unresolved_lookup_failure_is_unbound_symbol() {
        let map = SymbolMap::default();
        let symbol = Symbol::from("missing");
        let err = map.get_symbol_cell_with(symbol, |_| Ok(())).unwrap_err();

        assert!(matches!(err, RuntimeError::UnboundSymbol { symbol: s } if s == symbol));
    }

    #[test]
    fn test_symbol_map_get_or_init() {
        let mut map = SymbolMap::default();
        let sym = Symbol::from(":keyword");

        let cell = map.get_or_init_symbol(sym);
        assert!(cell.special); // Should detect ':' prefix
        assert!(cell.name.is_indexed());
    }
}

impl crate::gc::ConstructObject for SymbolMap {
    type Init<'a> = ();
    fn size_from_init(_init: &Self::Init<'_>) -> usize {
        std::mem::size_of::<crate::gc::Headered<Self>>()
    }
    fn size(&self) -> usize {
        std::mem::size_of::<crate::gc::Headered<Self>>()
    }
    unsafe fn init_fn(_init: Self::Init<'_>, _ctx: &crate::gc::GcContext) -> Self {
        Self::default()
    }
}
