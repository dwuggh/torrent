use proc_macros::Trace;

use crate::{
    core::{
        tagged_ptr::TaggedPtr,
        object::{LispType, Object},
    },
    gc::{Gc, GcInner},
};

#[derive(Clone, Trace, Debug)]
pub struct LispVector(pub Gc<Vector>);
pub type Vector = Vec<Object>;

impl_tagged_ptr_for_gc!(LispVector, LispType::Vector, Vec<Object>);
