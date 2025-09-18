use lasso::{Key, ThreadedRodeo};
use std::sync::LazyLock;

use crate::core::symbol::Symbol;

pub static INTERNER: LazyLock<Interner> = LazyLock::new(Interner::new);

#[derive(Debug, Clone, Copy)]
pub struct SpecialIdent {
    pub function: Ident,
    pub mcro: Ident,
    pub t: Ident,
    pub nil: Ident,
}

#[derive(Debug)]
pub struct Interner {
    interner: ThreadedRodeo<Ident>,
    pub special: SpecialIdent,
}

impl Interner {
    pub fn new() -> Self {
        let interner = ThreadedRodeo::new();
        let function = interner.get_or_intern("function");
        let mcro = interner.get_or_intern("mcro");
        let t = interner.get_or_intern("t");
        let nil = interner.get_or_intern("nil");
        let special = SpecialIdent {
            function,
            mcro,
            t,
            nil,
        };
        Self { interner, special }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident(pub u32);

unsafe impl Key for Ident {
    fn into_usize(self) -> usize {
        self.0 as usize
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        Some(Self(int as u32))
    }
}

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ident").field("text", &self.text()).finish()
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.text())
    }
}

impl Ident {
    pub fn new(key: u32) -> Self {
        // HACK put padding here to prevent null pointer check
        // tracing::debug!("spur: {spur:?}");
        Self(key)
    }

    pub fn text<'a>(&self) -> &'a str {
        INTERNER.interner.resolve(&self)
    }

    pub fn from_string(name: &str) -> Self {
        let key = INTERNER.interner.get_or_intern(name);
        key
    }

    pub fn from_raw(value: u32) -> Self {
        Self(value)
    }

    pub fn special() -> SpecialIdent {
        INTERNER.special
    }
}

impl From<u64> for Ident {
    fn from(value: u64) -> Self {
        Self(value as u32)
    }
}

impl From<Ident> for u64 {
    fn from(val: Ident) -> Self {
        val.0 as u64
    }
}

impl From<Ident> for i64 {
    fn from(val: Ident) -> Self {
        val.0 as i64
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self::from_string(value)
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self::from_string(&value)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.text()
    }
}

impl From<Symbol> for Ident {
    fn from(value: Symbol) -> Self {
        value.ident()
    }
}

impl From<&Symbol> for Ident {
    fn from(value: &Symbol) -> Self {
        value.ident()
    }
}

impl TryFrom<i64> for Ident {
    type Error = &'static str;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(Self::from(value as u64))
    }
}

impl AsRef<Ident> for Ident {
    fn as_ref(&self) -> &Ident {
        self
    }
}

impl AsMut<Ident> for Ident {
    fn as_mut(&mut self) -> &mut Ident {
        self
    }
}
