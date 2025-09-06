use std::collections::HashMap;

use proc_macros::Trace;

use crate::{
    core::value::{LispType, TaggedPtr, Value},
    gc::{Gc, GcInner},
};

#[derive(Clone, Trace, Debug)]
pub struct Map(pub(crate) Gc<HashMap<Value, Value>>);

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
    pub fn new() -> Self {
        Map(Gc::new(HashMap::new()))
    }

    pub fn len(&self) -> usize {
        self.0.get().len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.get().is_empty()
    }

    pub fn contains_key(&self, key: &Value) -> bool {
        self.0.get().contains_key(key)
    }

    pub fn get(&self, key: &Value) -> Option<Value> {
        self.0.get().get(key).cloned()
    }

    pub fn insert(&mut self, key: Value, value: Value) {
        self.0.get_mut().insert(key, value);
    }

    pub fn remove(&mut self, key: &Value) -> Option<Value> {
        self.0.get_mut().remove(key)
    }

    pub fn clear(&mut self) {
        self.0.get_mut().clear();
    }
}
