use std::collections::HashMap;

use proc_macros::Trace;

use crate::{
    core::Tagged,
    core::object::{LispType, Object},
    gc::Gc,
};

#[derive(Clone, Trace, Debug)]
pub struct LispHashTable(pub Gc<HashTable>);

#[derive(Clone, Trace, Debug)]
pub struct HashTable(HashMap<Object, Object>);

impl_tagged_for_gc!(LispHashTable, LispType::HashTable, HashTable);

impl Default for LispHashTable {
    fn default() -> Self {
        Self::new()
    }
}

impl LispHashTable {
    pub fn new() -> Self {
        LispHashTable(Gc::new(HashTable::new()))
    }
}

impl HashTable {
    pub fn new() -> Self {
        HashTable(HashMap::new())
    }
}
