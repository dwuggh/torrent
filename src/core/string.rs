use std::sync::Arc;

use proc_macros::Trace;

use crate::core::value::{LispType, TaggedPtr, Value};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LispString(Arc<String>);

impl LispString {
    pub fn new(string: impl Into<Arc<String>>) -> Self {
        Self(string.into())
    }

    pub fn from_str(str: impl AsRef<str>) -> Self {
        Self(Arc::from(str.as_ref().to_string()))
    }

    pub unsafe fn inc_strong_rc(&self) {
        Arc::increment_strong_count(&self.0);
    }
}

impl AsRef<Arc<String>> for LispString {
    fn as_ref(&self) -> &Arc<String> {
        &self.0
    }
}

impl AsRef<str> for LispString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl TaggedPtr for LispString {
    const TAG: LispType = LispType::Float;
    unsafe fn cast(val: u64) -> Self {
        let string = Arc::from_raw(val as *const String);
        Self::new(string)
    }

    // TODO
    unsafe fn get_untagged_data(self) -> u64 {
        self.0.as_ptr() as u64
    }
}
