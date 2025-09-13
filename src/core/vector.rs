use proc_macros::Trace;

use crate::{
    core::{
        tagged_ptr::Tagged,
        object::{LispType, Object},
    },
    gc::Gc,
};

#[derive(Clone, Trace, Debug)]
pub struct LispVector(pub Gc<Vector>);
pub type Vector = Vec<Object>;

impl_tagged_for_gc!(LispVector, LispType::Vector, Vec<Object>);
