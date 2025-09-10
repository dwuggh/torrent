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
        match self.cdr().as_ref() {
            crate::core::value::LispValueRef::Nil => true,
            crate::core::value::LispValueRef::Cons(next_cons) => next_cons.is_proper_list(),
            _ => false, // Improper list (dotted pair)
        }
    }

    /// Get the length of the list (if it's a proper list)
    pub fn length(&self) -> Option<usize> {
        let mut count = 1;
        let mut current = *self.cdr();

        loop {
            match current.as_ref() {
                crate::core::value::LispValueRef::Nil => return Some(count),
                crate::core::value::LispValueRef::Cons(next_cons) => {
                    current = *next_cons.cdr();
                    count += 1;
                }
                _ => return None, // Improper list
            }
        }
    }

    /// Convert the cons list to a Vec<Value> (if it's a proper list)
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        if !self.is_proper_list() {
            return None;
        }

        let mut result = Vec::new();
        let mut current = Value::from(self.clone());

        loop {
            match current.as_ref() {
                crate::core::value::LispValueRef::Cons(cons) => {
                    result.push(*cons.car());
                    current = *cons.cdr();
                }
                crate::core::value::LispValueRef::Nil => return Some(result),
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
        let mut current = Value::from(self.clone());
        let mut i = 0;

        loop {
            match current.as_ref() {
                crate::core::value::LispValueRef::Cons(cons) => {
                    if i == index {
                        return Some(*cons.car());
                    }
                    current = *cons.cdr();
                    i += 1;
                }
                crate::core::value::LispValueRef::Nil => return None,
                _ => return None, // Improper list
            }
        }
    }

    /// Append another list to this one (creates a new list)
    pub fn append(&self, other: Value) -> Self {
        match self.cdr().as_ref() {
            crate::core::value::LispValueRef::Nil => Cons::new(*self.car(), other),
            crate::core::value::LispValueRef::Cons(cdr_cons) => {
                Cons::new(*self.car(), cdr_cons.append(other).tag())
            }
            _ => {
                // Improper list, just replace the cdr
                Cons::new(*self.car(), other)
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

impl LispCons {
    /// Get the car (first element) of the cons cell
    pub fn car(&self) -> &Value {
        &self.car
    }

    /// Get the cdr (rest) of the cons cell
    pub fn cdr(&self) -> &Value {
        &self.cdr
    }

    /// Set the car (first element) of the cons cell
    pub fn set_car(&mut self, value: Value) {
        self.car = value;
    }

    /// Set the cdr (rest) of the cons cell
    pub fn set_cdr(&mut self, value: Value) {
        self.cdr = value;
    }

    /// Check if this cons cell is a proper list (ends with nil)
    pub fn is_proper_list(&self) -> bool {
        match self.cdr.as_ref() {
            crate::core::value::LispValueRef::Nil => true,
            crate::core::value::LispValueRef::Cons(next_cons) => next_cons.is_proper_list(),
            _ => false, // Improper list (dotted pair)
        }
    }

    /// Get the length of the list (if it's a proper list)
    pub fn length(&self) -> Option<usize> {
        let mut count = 1;
        let mut current = self.cdr;

        loop {
            match current.as_ref() {
                crate::core::value::LispValueRef::Nil => return Some(count),
                crate::core::value::LispValueRef::Cons(next_cons) => {
                    current = *next_cons.cdr();
                    count += 1;
                }
                _ => return None, // Improper list
            }
        }
    }

    /// Convert the cons list to a Vec<Value> (if it's a proper list)
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        if !self.is_proper_list() {
            return None;
        }

        let mut result = Vec::new();
        let mut current = Value::from(Cons(Gc::new(self.clone())));

        loop {
            match current.as_ref() {
                crate::core::value::LispValueRef::Cons(cons) => {
                    result.push(*cons.car());
                    current = *cons.cdr();
                }
                crate::core::value::LispValueRef::Nil => return Some(result),
                _ => unreachable!(), // We already checked it's a proper list
            }
        }
    }

    /// Create a cons list from a Vec<Value>
    pub fn from_vec(values: Vec<Value>) -> Option<LispCons> {
        if values.is_empty() {
            return None;
        }

        let mut result = LispCons {
            car: values[values.len() - 1],
            cdr: nil(),
        };

        for value in values.iter().rev().skip(1) {
            result = LispCons {
                car: *value,
                cdr: Cons(Gc::new(result)).tag(),
            };
        }

        Some(result)
    }

    /// Get the nth element of the list (0-indexed)
    pub fn nth(&self, index: usize) -> Option<Value> {
        let mut current = Value::from(Cons(Gc::new(self.clone())));
        let mut i = 0;

        loop {
            match current.as_ref() {
                crate::core::value::LispValueRef::Cons(cons) => {
                    if i == index {
                        return Some(*cons.car());
                    }
                    current = *cons.cdr();
                    i += 1;
                }
                crate::core::value::LispValueRef::Nil => return None,
                _ => return None, // Improper list
            }
        }
    }

    /// Append another list to this one (creates a new list)
    pub fn append(&self, other: Value) -> LispCons {
        match self.cdr.as_ref() {
            crate::core::value::LispValueRef::Nil => LispCons {
                car: self.car,
                cdr: other,
            },
            crate::core::value::LispValueRef::Cons(cdr_cons) => LispCons {
                car: self.car,
                cdr: Cons(Gc::new(cdr_cons.append(other))).tag(),
            },
            _ => {
                // Improper list, just replace the cdr
                LispCons {
                    car: self.car,
                    cdr: other,
                }
            }
        }
    }

    /// Reverse the list (if it's a proper list)
    pub fn reverse(&self) -> Option<LispCons> {
        let values = self.to_vec()?;
        let mut reversed = values;
        reversed.reverse();
        Self::from_vec(reversed)
    }
}
