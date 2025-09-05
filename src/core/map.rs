use std::collections::HashMap;

use proc_macros::Trace;

use crate::{core::value::{LispType, TaggedPtr, Value}, gc::{Gc, GcInner}};


#[derive(Clone, Trace, Debug)]
pub struct Map(Gc<HashMap<Value, Value>>);

impl TaggedPtr for Map {
    const TAG: LispType = LispType::Map;

    unsafe fn cast(val: u64) -> Self {
        Map(Gc::from_raw(val as *mut GcInner<HashMap<Value, Value>>))
    }

    unsafe fn get_untagged_data(self) -> u64 {
        Gc::into_raw(self.0) as u64
    }
}

impl Map {
}