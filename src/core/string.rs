use proc_macros::Trace;

use crate::core::Tagged;
use crate::core::object::LispType;
use crate::gc::Gc;

#[repr(align(16))]
#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub struct LispStr(pub Gc<Str>);

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub struct Str(String);

impl std::fmt::Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Str {
    pub fn new(_0: String) -> Self {
        Self(_0)
    }
}

impl_tagged_for_gc!(LispStr, LispType::Str, Str);

impl std::fmt::Display for LispStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.get().fmt(f)
    }
}

impl LispStr {
    pub fn new(string: impl Into<String>) -> Self {
        Self(Gc::new(Str::new(string.into())))
    }

    pub fn from_str(str: impl AsRef<str>) -> Self {
        Self::new(str.as_ref())
    }
}

impl AsRef<str> for LispStr {
    fn as_ref(&self) -> &str {
        self.0.get().as_ref()
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}
