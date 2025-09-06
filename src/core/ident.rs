use lasso::{Key, Spur, ThreadedRodeo};
use std::{marker::PhantomData, ops::Deref, sync::LazyLock};

pub static INTERNER: LazyLock<ThreadedRodeo> = LazyLock::new(|| ThreadedRodeo::new());

#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub(crate) spur: Spur,
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

impl Into<u64> for Ident {
    fn into(self) -> u64 {
        self.spur.into_usize() as u64
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
