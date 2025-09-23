use proc_macros::Trace;

use crate::core::Tagged;
use crate::{
    core::object::{LispType, Object},
    gc::Gc,
};

#[derive(Debug, Clone, Trace)]
pub struct Indirect(pub Gc<Object>);

impl Indirect {
    pub fn new(object: Object) -> Self {
        tracing::error!("creating indirect");
        Self(Gc::new(object))
    }
}
impl_tagged_for_gc!(Indirect, LispType::Indirect, Object);
