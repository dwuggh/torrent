use lasso::{Key, ThreadedRodeo};
use std::sync::LazyLock;

use crate::core::symbol::Symbol;

pub static INTERNER: LazyLock<ThreadedRodeo<Ident>> = LazyLock::new(ThreadedRodeo::new);

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident(pub u64);

unsafe impl Key for Ident {
    fn into_usize(self) -> usize {
        self.0 as usize
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        Some(Self(int as u64))
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
    pub fn new(key: u64) -> Self {
        // HACK put padding here to prevent null pointer check
        // tracing::debug!("spur: {spur:?}");
        Self(key)
    }

    pub fn text<'a>(&self) -> &'a str {
        INTERNER.resolve(&self)
    }

    pub fn from_string(name: &str) -> Self {
        let key = INTERNER.get_or_intern(name);
        key
    }
}

impl From<u64> for Ident {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl From<Ident> for u64 {
    fn from(val: Ident) -> Self {
        val.0
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
        value.name
    }
}

impl From<&Symbol> for Ident {
    fn from(value: &Symbol) -> Self {
        value.name
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
