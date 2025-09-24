use proc_macros::Trace;

use crate::{
    core::Tagged,
    core::{
        object::{nil, LispType, Object, ObjectRef},
        tagged_ptr::TaggedObj,
    },
    gc::Gc,
};

#[derive(Clone, Trace, Debug)]
pub struct LispCons(pub Gc<Cons>);

#[derive(Clone, Trace, Debug)]
pub struct Cons {
    car: Object,
    cdr: Object,
}

impl_tagged_for_gc!(LispCons, LispType::Cons, Cons);

impl LispCons {
    pub fn new(car: Object, cdr: Object) -> Self {
        Self(Gc::new(Cons { car, cdr }))
    }

    pub fn from_iter(mut values: impl Iterator<Item = Object>) -> Option<LispCons> {
        let car = values.next()?;
        let cdr = Self::from_iter(values)
            .map(|cons| cons.tag())
            .unwrap_or(nil());
        Some(LispCons::new(car, cdr))
    }

    /// Get the car (first element) of the cons cell
    pub fn car(&self) -> &Object {
        &self.0.get().car
    }

    /// Get the cdr (rest) of the cons cell
    pub fn cdr(&self) -> &Object {
        &self.0.get().cdr
    }

    /// Set the car (first element) of the cons cell
    pub fn set_car(&mut self, value: Object) {
        self.0.get_mut().car = value;
    }

    /// Set the cdr (rest) of the cons cell
    pub fn set_cdr(&mut self, value: Object) {
        self.0.get_mut().cdr = value;
    }
}

impl Cons {
    /// Get the car (first element) of the cons cell
    pub fn car(&self) -> &Object {
        &self.car
    }

    /// Get the cdr (rest) of the cons cell
    pub fn cdr(&self) -> &Object {
        &self.cdr
    }

    /// Set the car (first element) of the cons cell
    pub fn set_car(&mut self, value: Object) {
        self.car = value;
    }

    /// Set the cdr (rest) of the cons cell
    pub fn set_cdr(&mut self, value: Object) {
        self.cdr = value;
    }

    /// Check if this cons cell is a proper list (ends with nil)
    pub fn is_proper_list(&self) -> bool {
        match self.cdr.as_ref() {
            ObjectRef::Nil => true,
            ObjectRef::Cons(next_cons) => next_cons.is_proper_list(),
            _ => false, // Improper list (dotted pair)
        }
    }

    /// Get the length of the list (if it's a proper list)
    pub fn length(&self) -> Option<usize> {
        let mut count = 1;
        let mut current = self.cdr.as_ref();

        loop {
            match current {
                ObjectRef::Nil => return Some(count),
                ObjectRef::Cons(next_cons) => {
                    current = next_cons.cdr().as_ref();
                    count += 1;
                }
                _ => return None, // Improper list
            }
        }
    }

    /// Convert the cons list to a Vec<Value> (if it's a proper list)
    pub fn to_vec(&self) -> Option<Vec<Object>> {
        if !self.is_proper_list() {
            return None;
        }

        let mut result = Vec::new();
        let mut current = ObjectRef::Cons(self);

        loop {
            match current {
                ObjectRef::Cons(cons) => {
                    result.push(cons.car().clone());
                    current = cons.cdr().as_ref();
                }
                ObjectRef::Nil => return Some(result),
                _ => unreachable!(), // We already checked it's a proper list
            }
        }
    }

    /// Create a cons list from a Vec<Value>
    pub fn from_vec(mut values: Vec<Object>) -> Option<Cons> {
        if values.is_empty() {
            return None;
        }

        let mut result = Cons {
            car: values.pop().unwrap(),
            cdr: nil(),
        };

        for value in values.into_iter().rev() {
            result = Cons {
                car: value,
                cdr: LispCons(Gc::new(result)).tag(),
            };
        }

        Some(result)
    }

    /// Get the nth element of the list (0-indexed)
    pub fn nth(&self, index: usize) -> Option<&Object> {
        let mut current = ObjectRef::Cons(self);
        let mut i = 0;

        loop {
            match current {
                ObjectRef::Cons(cons) => {
                    if i == index {
                        return Some(cons.car());
                    }
                    current = cons.cdr().as_ref();
                    i += 1;
                }
                ObjectRef::Nil => return None,
                _ => return None, // Improper list
            }
        }
    }

    /// Append another list to this one (creates a new list)
    pub fn append(&self, other: Object) -> Cons {
        match self.cdr.as_ref() {
            ObjectRef::Nil => Cons {
                car: self.car.clone(),
                cdr: other,
            },
            ObjectRef::Cons(cdr_cons) => Cons {
                car: self.car.clone(),
                cdr: LispCons(Gc::new(cdr_cons.append(other))).tag(),
            },
            _ => {
                // Improper list, just replace the cdr
                Cons {
                    car: self.car.clone(),
                    cdr: other,
                }
            }
        }
    }

    /// Reverse the list (if it's a proper list)
    pub fn reverse(&self) -> Option<Cons> {
        let values = self.to_vec()?;
        let mut reversed = values;
        reversed.reverse();
        Self::from_vec(reversed)
    }
}

impl From<Cons> for Object {
    fn from(value: Cons) -> Self {
        LispCons(Gc::new(value)).tag()
    }
}
