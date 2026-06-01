//! Placeholder for a moving-GC-safe hash table.
//!
//! The old `HashMap<Object, Object>` representation is intentionally deferred:
//! moving collectors may rewrite object keys, which can invalidate hash buckets.

#[derive(Clone, Debug, Default)]
pub struct HashTable;

#[derive(Clone, Debug, Default)]
pub struct LispHashTable;
