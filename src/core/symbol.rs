use std::marker::PhantomData;
use std::sync::LazyLock;

// include!(concat!(env!("OUT_DIR"), "/sym.rs"));
// use sym::BUILTIN_SYMBOLS;

use dashmap::DashMap;
use proc_macros::Trace;

use crate::core::ident::Ident;
use crate::core::object::nil;
use crate::gc::Trace;
use crate::{core::object::Object, core::TaggedPtr, gc::Gc};

use super::object::LispType;

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
pub type LispSymbol = Symbol;

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

impl TryFrom<*mut Ident> for Symbol {
    type Error = ();

    fn try_from(value: *mut Ident) -> Result<Self, Self::Error> {
        Ok(unsafe {
            let ident: Ident = std::mem::transmute(value as u64);
            Self::new(ident)
        })
    }
}

impl TryFrom<*mut Symbol> for Symbol {
    type Error = ();

    fn try_from(value: *mut Symbol) -> Result<Self, Self::Error> {
        Ok(unsafe {
            tracing::debug!("symbol's tryinto: {:?}", value as u64);
            std::mem::transmute(value as u64)
        })
    }
}

impl TaggedPtr for Symbol {
    const TAG: LispType = LispType::Symbol;

    type Data = Symbol;

    type Inner = Symbol;

    unsafe fn to_raw(&self) -> u64 {
        self.name.into()
    }

    // fn raw(&self) -> u64 {
    //     unsafe { self.to_raw() | Self::TAG as u64 }
    // }

    // fn untag_ptr(val: u64) -> *mut Self::Inner {
    //     let untagged = val ^ Self::TAG as u64;
    //     tracing::info!("calling untag_ptr: untagged: {untagged}");
    //     // untagged as *mut Self::Inner
    //     unsafe { std::mem::transmute(untagged) }
    // }

    unsafe fn as_ref_unchecked(val: &Object) -> &Self::Data {
        unimplemented!()
    }

    // NOTE this function does not check for tag match
    unsafe fn as_mut_unchecked(val: &Object) -> &mut Self::Data {
        unimplemented!()
    }

    fn untag(val: Object) -> Result<Self, super::tagged_ptr::TaggedPtrError> {

        if super::tagged_ptr::get_tag(val.0 as i64) == Self::TAG {
            let sym = (&val).try_into().unwrap();
            std::mem::forget(val);
            Ok(sym)
        } else {
            Err(super::tagged_ptr::TaggedPtrError::TypeMisMatch)
        }
        
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
