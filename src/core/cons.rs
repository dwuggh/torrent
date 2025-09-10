use proc_macros::Trace;

use crate::{
    core::value::{nil, LispType, Value},
    core::TaggedPtr,
    gc::{Gc, GcInner},
};

#[derive(Clone, Trace, Debug)]
pub struct Cons(pub Gc<LispCons>);

#[derive(Clone, Trace, Debug)]
pub struct LispCons {
    car: Value,
    cdr: Value,
}

impl_tagged_ptr_for_gc!(Cons, LispType::Cons, LispCons);

impl Cons {
    pub fn new(car: Value, cdr: Value) -> Self {
        Self(Gc::new(LispCons { car, cdr }))
    }

    /// Get the car (first element) of the cons cell
    pub fn car(&self) -> &Value {
        &self.0.get().car
    }

    /// Get the cdr (rest) of the cons cell
    pub fn cdr(&self) -> &Value {
        &self.0.get().cdr
    }

    /// Set the car (first element) of the cons cell
    pub fn set_car(&mut self, value: Value) {
        self.0.get_mut().car = value;
    }

    /// Set the cdr (rest) of the cons cell
    pub fn set_cdr(&mut self, value: Value) {
        self.0.get_mut().cdr = value;
    }

    /// Check if this cons cell is a proper list (ends with nil)
    pub fn is_proper_list(&self) -> bool {
        match self.cdr().untag() {
            crate::core::value::LispValue::Nil => true,
            crate::core::value::LispValue::Cons(ref next_cons) => next_cons.is_proper_list(),
            _ => false, // Improper list (dotted pair)
        }
    }

    /// Get the length of the list (if it's a proper list)
    pub fn length(&self) -> Option<usize> {
        let mut count = 1;
        let mut current = self.clone();

        loop {
            match current.cdr().untag() {
                crate::core::value::LispValue::Nil => return Some(count),
                crate::core::value::LispValue::Cons(next_cons) => {
                    current = next_cons;
                    count += 1;
                }
                _ => unreachable!(), // We already checked it's a proper list
            }
        }
    }

    /// Convert the cons list to a Vec<Value> (if it's a proper list)
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        if !self.is_proper_list() {
            return None;
        }

        let mut result = Vec::new();
        let mut current = self.clone();

        loop {
            result.push(current.car());
            match current.cdr().untag() {
                crate::core::value::LispValue::Nil => return Some(result),
                crate::core::value::LispValue::Cons(next_cons) => {
                    current = next_cons;
                }
                _ => unreachable!(), // We already checked it's a proper list
            }
        }
    }

    /// Create a cons list from a Vec<Value>
    pub fn from_vec(values: Vec<Value>) -> Option<Self> {
        if values.is_empty() {
            return None;
        }

        let mut result = Cons::new(values[values.len() - 1], nil());

        for value in values.iter().rev().skip(1) {
            result = Cons::new(*value, result.tag());
        }

        Some(result)
    }

    /// Get the nth element of the list (0-indexed)
    pub fn nth(&self, index: usize) -> Option<Value> {
        let mut current = self.clone();
        let mut i = 0;

        loop {
            if i == index {
                return Some(current.car());
            }

            match current.cdr().untag() {
                crate::core::value::LispValue::Nil => return None,
                crate::core::value::LispValue::Cons(next_cons) => {
                    current = next_cons;
                    i += 1;
                }
                _ => return None, // Improper list
            }
        }
    }

    /// Append another list to this one (creates a new list)
    pub fn append(&self, other: Value) -> Self {
        match self.cdr().untag() {
            crate::core::value::LispValue::Nil => Cons::new(self.car(), other),
            crate::core::value::LispValue::Cons(cdr_cons) => {
                Cons::new(self.car(), cdr_cons.append(other).tag())
            }
            _ => {
                // Improper list, just replace the cdr
                Cons::new(self.car(), other)
            }
        }
    }

    /// Reverse the list (if it's a proper list)
    pub fn reverse(&self) -> Option<Self> {
        let values = self.to_vec()?;
        let mut reversed = values;
        reversed.reverse();
        Self::from_vec(reversed)
    }
}
