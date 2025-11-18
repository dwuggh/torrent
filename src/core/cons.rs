use proc_macros::{defun, Trace};
use crate::{
    core::Tagged,
    core::{
        object::{LispType, Object, ObjectRef, nil, tru},
        tagged_ptr::TaggedObj,
    },
    gc::Gc,
};
use crate::core::error::RuntimeResult as Result;
use crate::runtime_bail;
use crate::runtime_error;

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

#[defun]
fn cons(car: Object, cdr: Object) -> Object {
    LispCons::new(car, cdr).tag()
}

#[defun]
fn list(vals: &[Object]) -> Object {
    if vals.is_empty() {
        nil()
    } else {
        let iter = vals.iter().cloned();
        LispCons::from_iter(iter).map(|c| c.tag()).unwrap_or_else(nil)
    }
}

#[defun]
fn car(obj: Object) -> Result<Object> {
    match obj.as_ref() {
        ObjectRef::Nil => Ok(nil()),
        ObjectRef::Cons(cons_cell) => Ok(cons_cell.car().clone()),
        _ => runtime_bail!(CannotTakeCar, value: obj),
    }
}

#[defun]
fn cdr(obj: Object) -> Result<Object> {
    match obj.as_ref() {
        ObjectRef::Nil => Ok(nil()),
        ObjectRef::Cons(cons_cell) => Ok(cons_cell.cdr().clone()),
        _ => runtime_bail!(CannotTakeCdr, value: obj),
    }
}

#[defun]
fn setcar(cell: &mut Cons, newcar: Object) -> Object {
    cell.set_car(newcar.clone());
    newcar
}

#[defun]
fn setcdr(cell: &mut Cons, newcdr: Object) -> Object {
    cell.set_cdr(newcdr.clone());
    newcdr
}

#[defun]
fn consp(obj: Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Cons(_) => tru(),
        _ => nil(),
    }
}

#[defun]
fn listp(obj: Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Cons(_) | ObjectRef::Nil => tru(),
        _ => nil(),
    }
}

#[defun]
fn atom(obj: Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Cons(_) => nil(),
        _ => tru(),
    }
}

// not moved to src/fns.rs
#[defun]
fn length(list: Object) -> Result<Object> {
    match list.as_ref() {
        ObjectRef::Nil => Ok(crate::core::number::LispInteger(0).tag()),
        ObjectRef::Cons(cons_cell) => {
            if let Some(len) = cons_cell.length() {
                Ok(crate::core::number::LispInteger(len as i64).tag())
            } else { runtime_bail!(NotAList, value: list) }
        }
        _ => runtime_bail!(NotAList, value: list),
    }
}

#[defun]
fn nth(n: i64, list: Object) -> Result<Object> {
    if n < 0 { return Ok(nil()); }
    let mut idx = n as usize;
    let mut cur = list;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                if idx == 0 { return Ok(cons_cell.car().clone()); }
                idx -= 1;
                cur = cons_cell.cdr().clone();
            }
            _ => return runtime_bail!(NotAList, value: cur),
        }
    }
}

#[defun]
fn nthcdr(n: i64, list: Object) -> Result<Object> {
    if n < 0 { return Ok(list); }
    let mut idx = n as usize;
    let mut cur = list;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                if idx == 0 { return Ok(cur); }
                idx -= 1;
                cur = cons_cell.cdr().clone();
            }
            _ => return runtime_bail!(NotAList, value: cur),
        }
    }
}

#[defun]
fn append(list: Object, tail: Object) -> Result<Object> {
    match list.as_ref() {
        ObjectRef::Nil => Ok(tail),
        ObjectRef::Cons(cons_cell) => {
            let result = cons_cell.append(tail);
            Ok(Object::from(result))
        }
        _ => runtime_bail!(NotAList, value: list),
    }
}

#[defun]
fn reverse(list: Object) -> Result<Object> {
    match list.as_ref() {
        ObjectRef::Nil => Ok(nil()),
        ObjectRef::Cons(cons_cell) => {
            let Some(res) = cons_cell.reverse() else { return runtime_bail!(NotAList, value: list) };
            Ok(Object::from(res))
        }
        _ => runtime_bail!(NotAList, value: list),
    }
}

// eq/equal moved to src/data.rs

#[defun]
fn memq(elt: Object, list: Object) -> Result<Object> {
    let mut cur = list;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                if cons_cell.car().0 == elt.0 { return Ok(cur); }
                cur = cons_cell.cdr().clone();
            }
            _ => return runtime_bail!(NotAList, value: cur),
        }
    }
}

#[defun]
fn assoc(key: Object, alist: Object) -> Result<Object> {
    let mut cur = alist;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                match cons_cell.car().as_ref() {
                    ObjectRef::Cons(pair) => {
                        if pair.car().0 == key.0 { return Ok(cons_cell.car().clone()); }
                    }
                    _ => {}
                }
                cur = cons_cell.cdr().clone();
            }
            _ => return runtime_bail!(NotAList, value: cur),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::number::LispInteger;
    use crate::core::object::{ObjectRef, nil};
    use crate::core::symbol::Symbol;

    #[test]
    fn test_cons_basic() {
        let car = LispInteger(42).tag();
        let cdr = nil();
        let pair = super::cons(car.clone(), cdr.clone());

        match pair.as_ref() {
            ObjectRef::Cons(cons_cell) => {
                let ObjectRef::Int(v) = cons_cell.car().as_ref() else { panic!("car not int") };
                assert_eq!(v, 42);
                assert!(matches!(cons_cell.cdr().as_ref(), ObjectRef::Nil));
            }
            _ => panic!("cons did not return a cons cell"),
        }
    }

    #[test]
    fn test_list_empty() {
        let lst = super::list(&[]);
        assert!(matches!(lst.as_ref(), ObjectRef::Nil));
    }

    #[test]
    fn test_list_multiple() {
        let vals = vec![LispInteger(1).tag(), LispInteger(2).tag(), LispInteger(3).tag()];
        let lst = super::list(&vals);
        match lst.as_ref() {
            ObjectRef::Cons(cons_cell) => {
                let v = cons_cell.to_vec().expect("proper list");
                assert_eq!(v.len(), 3);
                let ints: Vec<i64> = v
                    .into_iter()
                    .map(|o| {
                        let ObjectRef::Int(i) = o.as_ref() else { panic!("not int") };
                        i
                    })
                    .collect();
                assert_eq!(ints, vec![1, 2, 3]);
            }
            _ => panic!("list did not return cons"),
        }
    }

    #[test]
    fn test_car_cdr() {
        let pair = super::cons(LispInteger(10).tag(), LispInteger(20).tag());
        let car = super::car(pair.clone()).unwrap();
        let cdr = super::cdr(pair.clone()).unwrap();
        let ObjectRef::Int(v1) = car.as_ref() else { panic!("car not int") };
        assert_eq!(v1, 10);
        let ObjectRef::Int(v2) = cdr.as_ref() else { panic!("cdr not int") };
        assert_eq!(v2, 20);

        // (car nil) and (cdr nil) => nil
        assert!(matches!(super::car(nil()).unwrap().as_ref(), ObjectRef::Nil));
        assert!(matches!(super::cdr(nil()).unwrap().as_ref(), ObjectRef::Nil));
    }

    #[test]
    fn test_setcar_setcdr() {
        let mut cell = LispCons::new(LispInteger(1).tag(), LispInteger(2).tag());
        super::setcar(&mut cell.0.get_mut(), LispInteger(3).tag());
        super::setcdr(&mut cell.0.get_mut(), LispInteger(4).tag());
        let cons_obj = cell.tag();
        match cons_obj.as_ref() {
            ObjectRef::Cons(c) => {
                let ObjectRef::Int(a) = c.car().as_ref() else { panic!("car not int") };
                let ObjectRef::Int(d) = c.cdr().as_ref() else { panic!("cdr not int") };
                assert_eq!(a, 3);
                assert_eq!(d, 4);
            }
            _ => panic!("not cons"),
        }
    }

    #[test]
    fn test_predicates() {
        let lst = super::list(&[LispInteger(1).tag()]);
        assert!(matches!(super::consp(lst.clone()).as_ref(), ObjectRef::True));
        assert!(matches!(super::listp(lst.clone()).as_ref(), ObjectRef::True));
        assert!(matches!(super::listp(nil()).as_ref(), ObjectRef::True));
        assert!(matches!(super::atom(LispInteger(1).tag()).as_ref(), ObjectRef::True));
        // assert!(matches!(super::null(nil()).as_ref(), ObjectRef::True));
    }

    #[test]
    fn test_length_nth_nthcdr() {
        let lst = super::list(&[LispInteger(1).tag(), LispInteger(2).tag(), LispInteger(3).tag()]);
        let len = super::length(lst.clone()).unwrap();
        let ObjectRef::Int(l) = len.as_ref() else { panic!("len not int") };
        assert_eq!(l, 3);

        let second = super::nth(1, lst.clone()).unwrap();
        let ObjectRef::Int(v) = second.as_ref() else { panic!("nth not int") };
        assert_eq!(v, 2);

        let rest = super::nthcdr(2, lst.clone()).unwrap();
        match rest.as_ref() {
            ObjectRef::Cons(c) => {
                let ObjectRef::Int(v) = c.car().as_ref() else { panic!("not int") };
                assert_eq!(v, 3);
            }
            _ => panic!("nthcdr not cons"),
        }
    }

    #[test]
    fn test_append_reverse() {
        let a = super::list(&[LispInteger(1).tag(), LispInteger(2).tag()]);
        let b = super::list(&[LispInteger(3).tag(), LispInteger(4).tag()]);
        let app = super::append(a, b).unwrap();
        let ints: Vec<i64> = match app.as_ref() {
            ObjectRef::Cons(c) => c
                .to_vec()
                .unwrap()
                .into_iter()
                .map(|o| if let ObjectRef::Int(i) = o.as_ref() { i } else { panic!("not int") })
                .collect(),
            _ => panic!("not cons"),
        };
        assert_eq!(ints, vec![1, 2, 3, 4]);

        let rev = super::reverse(super::list(&[LispInteger(1).tag(), LispInteger(2).tag(), LispInteger(3).tag()])).unwrap();
        let ints: Vec<i64> = match rev.as_ref() {
            ObjectRef::Cons(c) => c
                .to_vec()
                .unwrap()
                .into_iter()
                .map(|o| if let ObjectRef::Int(i) = o.as_ref() { i } else { panic!("not int") })
                .collect(),
            _ => panic!("not cons"),
        };
        assert_eq!(ints, vec![3, 2, 1]);
    }

    #[test]
    fn test_memq_assoc() {
        // memq
        let lst = super::list(&[LispInteger(1).tag(), LispInteger(2).tag(), LispInteger(3).tag()]);
        let m = super::memq(LispInteger(2).tag(), lst).unwrap();
        match m.as_ref() {
            ObjectRef::Cons(c) => {
                let ObjectRef::Int(v) = c.car().as_ref() else { panic!("not int") };
                assert_eq!(v, 2);
            }
            _ => panic!("memq not cons"),
        }

        // assoc
        let key_a = Symbol::from("a").tag();
        let key_b = Symbol::from("b").tag();
        let pair_a = super::cons(key_a.clone(), LispInteger(1).tag());
        let pair_b = super::cons(key_b.clone(), LispInteger(2).tag());
        let alist = super::list(&[pair_a, pair_b.clone()]);
        let found = super::assoc(key_b, alist).unwrap();
        match found.as_ref() {
            ObjectRef::Cons(p) => {
                let ObjectRef::Int(v) = p.cdr().as_ref() else { panic!("not int") };
                assert_eq!(v, 2);
            }
            _ => panic!("assoc not cons pair"),
        }
    }
}
