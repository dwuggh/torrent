use proc_macros::Trace;

use crate::{core::value::Value, gc::Gc};


#[derive(Clone, Trace, Debug)]
pub struct Cons(pub Gc<ConsInner>);

#[derive(Clone, Trace, Debug)]
pub struct ConsInner {
    car: Value,
    cdr: Value,
}

impl Cons {
    pub fn new(car: Value, cdr: Value) -> Self {
        Self(Gc::new(ConsInner { car, cdr }))
    }

}