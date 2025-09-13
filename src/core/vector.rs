use proc_macros::Trace;

use crate::{
    core::{
        object::{LispType, Object},
        tagged_ptr::Tagged,
    },
    gc::Gc,
};

#[derive(Clone, Trace, Debug)]
pub struct LispVector(pub Gc<Vector>);
pub type Vector = Vec<Object>;

impl_tagged_for_gc!(LispVector, LispType::Vector, Vec<Object>);
