use proc_macros::Trace;

use crate::{core::value::{Value, TaggedPtr, LispType}, gc::Gc};


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

    /// Get the car (first element) of the cons cell
    pub fn car(&self) -> Value {
        self.0.get().car
    }

    /// Get the cdr (rest) of the cons cell
    pub fn cdr(&self) -> Value {
        self.0.get().cdr
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
        let mut current = *self;
        loop {
            match current.cdr().untag() {
                crate::core::value::LispValue::Nil => return true,
                crate::core::value::LispValue::Cons(next_cons) => {
                    current = next_cons;
                }
                _ => return false, // Improper list (dotted pair)
            }
        }
    }

    /// Get the length of the list (if it's a proper list)
    pub fn length(&self) -> Option<usize> {
        if !self.is_proper_list() {
            return None;
        }

        let mut count = 1;
        let mut current = *self;
        
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
        let mut current = *self;
        
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

        let mut result = Cons::new(values[values.len() - 1], Value::nil());
        
        for value in values.iter().rev().skip(1) {
            result = Cons::new(*value, result.tag());
        }
        
        Some(result)
    }

    /// Get the nth element of the list (0-indexed)
    pub fn nth(&self, index: usize) -> Option<Value> {
        let mut current = *self;
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
            crate::core::value::LispValue::Nil => {
                Cons::new(self.car(), other)
            }
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

impl TaggedPtr for Cons {
    const TAG: LispType = LispType::Cons;

    unsafe fn cast(val: u64) -> Self {
        Cons(Gc::from_raw(val as *mut crate::gc::GcInner<ConsInner>))
    }

    unsafe fn get_untagged_data(self) -> u64 {
        Gc::into_raw(self.0) as u64
    }
}
