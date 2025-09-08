use lasso::{Key, Spur, ThreadedRodeo};
use std::sync::LazyLock;

use crate::core::symbol::Symbol;

pub static INTERNER: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::new);

#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub(crate) spur: Spur,
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
    pub fn new(spur: Spur) -> Self {
        Self { spur }
    }

    pub fn text<'a>(&self) -> &'a str {
        INTERNER.resolve(&self.spur)
    }

    pub fn from_string(name: &str) -> Self {
        let spur = INTERNER.get_or_intern(name);
        Self::new(spur)
    }
}

impl From<u64> for Ident {
    fn from(value: u64) -> Self {
        let spur = Spur::try_from_usize(value as usize).unwrap();
        Self { spur }
    }
}

impl From<Ident> for u64 {
    fn from(val: Ident) -> Self {
        val.spur.into_usize() as u64
    }
}

impl From<Ident> for i64 {
    fn from(val: Ident) -> Self {
        val.spur.into_usize() as i64
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
