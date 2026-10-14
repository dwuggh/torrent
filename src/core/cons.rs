use std::ptr::NonNull;

use crate::core::error::RuntimeResult as Result;
use crate::runtime_bail;
use crate::runtime_error;
use crate::{
    core::{
        object::{Object, ObjectRef, nil, tru},
        tag::Tag,
    },
    gc::{Gc, GcContext, HeapObject, Rooted, RootedSlice, Trace, Visitor},
};
use proc_macros::{HeapEntries, defun};

#[derive(Clone, Debug)]
pub struct LispCons(pub Gc<Cons>);

#[derive(Clone, Debug, PartialEq, Eq, HeapEntries)]
pub struct Cons {
    car: Object,
    cdr: Object,
}

impl crate::gc::Tagged for Cons {
    const TAG: u8 = crate::gc::GcTag::CONS;
}

unsafe impl Trace for Cons {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        // Cons is compact `[car][cdr]`; both words are tagged object slots.
        unsafe { self.car.trace(visitor) };
        unsafe { self.cdr.trace(visitor) };
    }
}

unsafe impl HeapObject for Cons {
    const PRIMARY_TAG: u8 = crate::gc::GcTag::CONS;

    unsafe fn init(start: crate::gc::mmtk::util::Address, value: Self) {
        unsafe { start.store::<Cons>(value) };
    }

    unsafe fn data_from_object_ref(
        object: crate::gc::mmtk::util::ObjectReference,
    ) -> NonNull<Self> {
        let start = object.to_raw_address().sub(crate::gc::OBJECT_REF_OFFSET);
        unsafe { NonNull::new_unchecked(start.to_mut_ptr::<Cons>()) }
    }

    unsafe fn object_ref_from_data(data: NonNull<Self>) -> crate::gc::mmtk::util::ObjectReference {
        let start = crate::gc::mmtk::util::Address::from_ptr(data.as_ptr());
        unsafe {
            crate::gc::mmtk::util::ObjectReference::from_raw_address_unchecked(
                start + crate::gc::OBJECT_REF_OFFSET,
            )
        }
    }
}

impl_tagged_for_gc!(LispCons, Cons);

impl LispCons {
    pub fn new(car: Object, cdr: Object, ctx: &mut GcContext) -> Self {
        let car = ctx.root(car);
        let cdr = ctx.root(cdr);
        Self::new_rooted(&car, &cdr, ctx)
    }

    pub fn new_rooted(car: &Rooted, cdr: &Rooted, ctx: &mut GcContext) -> Self {
        Self(ctx.alloc::<Cons>((car, cdr)))
    }

    pub fn list_from_rooted_slice(values: &RootedSlice, ctx: &mut GcContext) -> Object {
        let result = ctx.root(nil());

        for value in values.iter().rev() {
            let cons = Self::new_rooted(&value, &result, ctx).tag();
            result.set(cons, ctx);
        }

        *result.get(ctx)
    }

    pub fn from_iter(values: impl Iterator<Item = Object>, ctx: &mut GcContext) -> Option<Self> {
        let values = values.collect::<Vec<_>>();
        let roots = ctx.root_slice(&values);
        <Self as crate::core::tag::Untag>::untag(Self::list_from_rooted_slice(&roots, ctx)).ok()
    }

    /// Get the car (first element) of the cons cell
    pub fn car(&self) -> &Object {
        &self.0.as_ref().car
    }

    /// Get the cdr (rest) of the cons cell
    pub fn cdr(&self) -> &Object {
        &self.0.as_ref().cdr
    }

    /// Set the car (first element) of the cons cell
    pub fn set_car(&mut self, value: Object) {
        self.0.as_mut().set_car(value);
    }

    /// Set the cdr (rest) of the cons cell
    pub fn set_cdr(&mut self, value: Object) {
        self.0.as_mut().set_cdr(value);
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
        self.car_mut().set(value);
    }

    /// Set the cdr (rest) of the cons cell
    pub fn set_cdr(&mut self, value: Object) {
        self.cdr_mut().set(value);
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
}

impl crate::gc::ConstructObject for Cons {
    type Init<'a> = (&'a Rooted, &'a Rooted);
    fn size_from_init(_init: &Self::Init<'_>) -> usize {
        std::mem::size_of::<Self>()
    }
    fn size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
    unsafe fn init_fn((car, cdr): Self::Init<'_>, ctx: &GcContext) -> Self {
        Self {
            car: *car.get(ctx),
            cdr: *cdr.get(ctx),
        }
    }
}

#[defun]
fn cons(car: &Rooted, cdr: &Rooted, ctx: &mut GcContext) -> Object {
    LispCons::new_rooted(car, cdr, ctx).tag()
}

#[defun]
fn list(vals: &RootedSlice, ctx: &mut GcContext) -> Object {
    LispCons::list_from_rooted_slice(vals, ctx)
}

#[defun]
fn car(obj: &Object) -> Result<Object> {
    let obj = *obj;
    match obj.as_ref() {
        ObjectRef::Nil => Ok(nil()),
        ObjectRef::Cons(cons_cell) => Ok(cons_cell.car().clone()),
        _ => runtime_bail!(CannotTakeCar, value: obj),
    }
}

#[defun]
fn cdr(obj: &Object) -> Result<Object> {
    let obj = *obj;
    match obj.as_ref() {
        ObjectRef::Nil => Ok(nil()),
        ObjectRef::Cons(cons_cell) => Ok(cons_cell.cdr().clone()),
        _ => runtime_bail!(CannotTakeCdr, value: obj),
    }
}

#[defun]
fn setcar(cell: &Object, newcar: &Object) -> Result<Object> {
    let cell_obj = *cell;
    let newcar = *newcar;
    let cell: &mut Cons = (&cell_obj)
        .try_into()
        .map_err(|_| crate::core::error::RuntimeError::wrong_type("cons", cell_obj.get_tag()))?;
    cell.car_mut().set(newcar);
    Ok(newcar)
}

#[defun]
fn setcdr(cell: &Object, newcdr: &Object) -> Result<Object> {
    let cell_obj = *cell;
    let newcdr = *newcdr;
    let cell: &mut Cons = (&cell_obj)
        .try_into()
        .map_err(|_| crate::core::error::RuntimeError::wrong_type("cons", cell_obj.get_tag()))?;
    cell.cdr_mut().set(newcdr);
    Ok(newcdr)
}

#[defun]
fn consp(obj: &Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Cons(_) => tru(),
        _ => nil(),
    }
}

#[defun]
fn listp(obj: &Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Cons(_) | ObjectRef::Nil => tru(),
        _ => nil(),
    }
}

#[defun]
fn atom(obj: &Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Cons(_) => nil(),
        _ => tru(),
    }
}

// not moved to src/fns.rs
#[defun]
fn length(list: &Object) -> Result<Object> {
    let list = *list;
    match list.as_ref() {
        ObjectRef::Nil => Ok(crate::core::number::LispInteger(0).tag()),
        ObjectRef::Cons(cons_cell) => {
            if let Some(len) = cons_cell.length() {
                Ok(crate::core::number::LispInteger(len as i64).tag())
            } else {
                runtime_bail!(NotAList, value: list)
            }
        }
        _ => runtime_bail!(NotAList, value: list),
    }
}

#[defun]
fn nth(n: i64, list: &Object) -> Result<Object> {
    if n < 0 {
        return Ok(nil());
    }
    let mut idx = n as usize;
    let mut cur = *list;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                if idx == 0 {
                    return Ok(cons_cell.car().clone());
                }
                idx -= 1;
                cur = cons_cell.cdr().clone();
            }
            _ => runtime_bail!(NotAList, value: cur),
        }
    }
}

#[defun]
fn nthcdr(n: i64, list: &Object) -> Result<Object> {
    let list = *list;
    if n < 0 {
        return Ok(list);
    }
    let mut idx = n as usize;
    let mut cur = list;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                if idx == 0 {
                    return Ok(cur);
                }
                idx -= 1;
                cur = cons_cell.cdr().clone();
            }
            _ => runtime_bail!(NotAList, value: cur),
        }
    }
}

#[defun]
fn append(list: &Rooted, tail: &Rooted, ctx: &mut GcContext) -> Result<Object> {
    let values = match list.get(ctx).as_ref() {
        ObjectRef::Nil => return Ok(*tail.get(ctx)),
        ObjectRef::Cons(cons_cell) => {
            let mut values = Vec::new();
            let mut cur = ObjectRef::Cons(cons_cell);

            loop {
                match cur {
                    ObjectRef::Cons(cons) => {
                        values.push(cons.car().clone());
                        cur = cons.cdr().as_ref();
                    }
                    ObjectRef::Nil => break values,
                    _ => break values,
                }
            }
        }
        _ => runtime_bail!(NotAList, value: *list.get(ctx)),
    };

    let value_roots = ctx.root_slice(&values);
    let tail = *tail.get(ctx);
    let result = ctx.root(tail);

    for value in value_roots.iter().rev() {
        let cons = LispCons::new_rooted(&value, &result, ctx).tag();
        result.set(cons, ctx);
    }

    Ok(*result.get(ctx))
}

#[defun]
fn reverse(list: &Rooted, ctx: &mut GcContext) -> Result<Object> {
    let list_obj = *list.get(ctx);
    let mut raw_values = match list_obj.as_ref() {
        ObjectRef::Nil => return Ok(nil()),
        ObjectRef::Cons(cons_cell) => {
            let Some(values) = cons_cell.to_vec() else {
                runtime_bail!(NotAList, value: list_obj);
            };
            values
        }
        _ => runtime_bail!(NotAList, value: list_obj),
    };
    raw_values.reverse();
    let roots = ctx.root_slice(&raw_values);
    Ok(LispCons::list_from_rooted_slice(&roots, ctx))
}

// eq/equal moved to src/data.rs

#[defun]
fn memq(elt: &Object, list: &Object) -> Result<Object> {
    let elt = *elt;
    let mut cur = *list;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                if cons_cell.car().0 == elt.0 {
                    return Ok(cur);
                }
                cur = cons_cell.cdr().clone();
            }
            _ => runtime_bail!(NotAList, value: cur),
        }
    }
}

#[defun]
fn assoc(key: &Object, alist: &Object) -> Result<Object> {
    let key = *key;
    let mut cur = *alist;
    loop {
        match cur.as_ref() {
            ObjectRef::Nil => return Ok(nil()),
            ObjectRef::Cons(cons_cell) => {
                match cons_cell.car().as_ref() {
                    ObjectRef::Cons(pair) => {
                        if pair.car().0 == key.0 {
                            return Ok(cons_cell.car().clone());
                        }
                    }
                    _ => {}
                }
                cur = cons_cell.cdr().clone();
            }
            _ => runtime_bail!(NotAList, value: cur),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::number::LispInteger;
    use crate::core::object::{ObjectRef, nil};
    use crate::core::symbol::Symbol;

    fn bind_test_mutator() -> crate::gc::test_support::MutatorGuard {
        crate::gc::test_support::bind_nogc_mutator()
    }

    fn cons_from(car: Object, cdr: Object) -> Object {
        let mut ctx = GcContext::new();
        let car = ctx.root(car);
        let cdr = ctx.root(cdr);
        super::cons(&car, &cdr, &mut ctx)
    }

    fn list_from(values: Vec<Object>) -> Object {
        let mut ctx = GcContext::new();
        let roots = ctx.root_slice(&values);
        super::list(&roots, &mut ctx)
    }

    fn append_from(list: Object, tail: Object) -> Result<Object> {
        let mut ctx = GcContext::new();
        let list = ctx.root(list);
        let tail = ctx.root(tail);
        super::append(&list, &tail, &mut ctx)
    }

    fn reverse_from(list: Object) -> Result<Object> {
        let mut ctx = GcContext::new();
        let root = ctx.root(list);
        super::reverse(&root, &mut ctx)
    }

    #[test]
    fn test_cons_basic() {
        let _mutator = bind_test_mutator();
        let car = LispInteger(42).tag();
        let cdr = nil();
        let pair = cons_from(car.clone(), cdr.clone());

        match pair.as_ref() {
            ObjectRef::Cons(cons_cell) => {
                let ObjectRef::Int(v) = cons_cell.car().as_ref() else {
                    panic!("car not int")
                };
                assert_eq!(v, 42);
                assert!(matches!(cons_cell.cdr().as_ref(), ObjectRef::Nil));
            }
            _ => panic!("cons did not return a cons cell"),
        }
    }

    #[test]
    fn test_list_empty() {
        let _mutator = bind_test_mutator();
        let lst = list_from(vec![]);
        assert!(matches!(lst.as_ref(), ObjectRef::Nil));
    }

    #[test]
    fn test_list_multiple() {
        let _mutator = bind_test_mutator();
        let vals = vec![
            LispInteger(1).tag(),
            LispInteger(2).tag(),
            LispInteger(3).tag(),
        ];
        let lst = list_from(vals);
        match lst.as_ref() {
            ObjectRef::Cons(cons_cell) => {
                let v = cons_cell.to_vec().expect("proper list");
                assert_eq!(v.len(), 3);
                let ints: Vec<i64> = v
                    .into_iter()
                    .map(|o| {
                        let ObjectRef::Int(i) = o.as_ref() else {
                            panic!("not int")
                        };
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
        let _mutator = bind_test_mutator();
        let pair = cons_from(LispInteger(10).tag(), LispInteger(20).tag());
        let car = super::car(&pair).unwrap();
        let cdr = super::cdr(&pair).unwrap();
        let ObjectRef::Int(v1) = car.as_ref() else {
            panic!("car not int")
        };
        assert_eq!(v1, 10);
        let ObjectRef::Int(v2) = cdr.as_ref() else {
            panic!("cdr not int")
        };
        assert_eq!(v2, 20);

        // (car nil) and (cdr nil) => nil
        assert!(matches!(
            super::car(&nil()).unwrap().as_ref(),
            ObjectRef::Nil
        ));
        assert!(matches!(
            super::cdr(&nil()).unwrap().as_ref(),
            ObjectRef::Nil
        ));
    }

    #[test]
    fn test_setcar_setcdr() {
        let _mutator = bind_test_mutator();
        let cell = LispCons::new(
            LispInteger(1).tag(),
            LispInteger(2).tag(),
            &mut GcContext::new(),
        );
        let cons_obj = cell.tag();
        super::setcar(&cons_obj, &LispInteger(3).tag()).unwrap();
        super::setcdr(&cons_obj, &LispInteger(4).tag()).unwrap();
        match cons_obj.as_ref() {
            ObjectRef::Cons(c) => {
                let ObjectRef::Int(a) = c.car().as_ref() else {
                    panic!("car not int")
                };
                let ObjectRef::Int(d) = c.cdr().as_ref() else {
                    panic!("cdr not int")
                };
                assert_eq!(a, 3);
                assert_eq!(d, 4);
            }
            _ => panic!("not cons"),
        }
    }

    #[test]
    fn generated_entries_update_car_and_cdr() {
        let _mutator = bind_test_mutator();
        let mut cell = LispCons::new(
            LispInteger(1).tag(),
            LispInteger(2).tag(),
            &mut GcContext::new(),
        );
        let new_car = LispInteger(5).tag();
        let new_cdr = LispInteger(6).tag();

        cell.0.as_mut().car_mut().set(new_car);
        cell.0.as_mut().cdr_mut().set(new_cdr);

        assert_eq!(*cell.car(), new_car);
        assert_eq!(*cell.cdr(), new_cdr);
    }

    #[test]
    fn test_predicates() {
        let _mutator = bind_test_mutator();
        let lst = list_from(vec![LispInteger(1).tag()]);
        assert!(matches!(
            super::consp(&lst.clone()).as_ref(),
            ObjectRef::True
        ));
        assert!(matches!(
            super::listp(&lst.clone()).as_ref(),
            ObjectRef::True
        ));
        assert!(matches!(super::listp(&nil()).as_ref(), ObjectRef::True));
        assert!(matches!(
            super::atom(&LispInteger(1).tag()).as_ref(),
            ObjectRef::True
        ));
        // assert!(matches!(super::null(nil()).as_ref(), ObjectRef::True));
    }

    #[test]
    fn test_length_nth_nthcdr() {
        let _mutator = bind_test_mutator();
        let lst = list_from(vec![
            LispInteger(1).tag(),
            LispInteger(2).tag(),
            LispInteger(3).tag(),
        ]);
        let len = super::length(&lst.clone()).unwrap();
        let ObjectRef::Int(l) = len.as_ref() else {
            panic!("len not int")
        };
        assert_eq!(l, 3);

        let second = super::nth(1, &lst.clone()).unwrap();
        let ObjectRef::Int(v) = second.as_ref() else {
            panic!("nth not int")
        };
        assert_eq!(v, 2);

        let rest = super::nthcdr(2, &lst.clone()).unwrap();
        match rest.as_ref() {
            ObjectRef::Cons(c) => {
                let ObjectRef::Int(v) = c.car().as_ref() else {
                    panic!("not int")
                };
                assert_eq!(v, 3);
            }
            _ => panic!("nthcdr not cons"),
        }
    }

    #[test]
    fn test_append_reverse() {
        let _mutator = bind_test_mutator();
        let a = list_from(vec![LispInteger(1).tag(), LispInteger(2).tag()]);
        let b = list_from(vec![LispInteger(3).tag(), LispInteger(4).tag()]);
        let app = append_from(a, b).unwrap();
        let ints: Vec<i64> = match app.as_ref() {
            ObjectRef::Cons(c) => c
                .to_vec()
                .unwrap()
                .into_iter()
                .map(|o| {
                    if let ObjectRef::Int(i) = o.as_ref() {
                        i
                    } else {
                        panic!("not int")
                    }
                })
                .collect(),
            _ => panic!("not cons"),
        };
        assert_eq!(ints, vec![1, 2, 3, 4]);

        let rev = reverse_from(list_from(vec![
            LispInteger(1).tag(),
            LispInteger(2).tag(),
            LispInteger(3).tag(),
        ]))
        .unwrap();
        let ints: Vec<i64> = match rev.as_ref() {
            ObjectRef::Cons(c) => c
                .to_vec()
                .unwrap()
                .into_iter()
                .map(|o| {
                    if let ObjectRef::Int(i) = o.as_ref() {
                        i
                    } else {
                        panic!("not int")
                    }
                })
                .collect(),
            _ => panic!("not cons"),
        };
        assert_eq!(ints, vec![3, 2, 1]);
    }

    #[test]
    fn test_memq_assoc() {
        let _mutator = bind_test_mutator();
        // memq
        let lst = list_from(vec![
            LispInteger(1).tag(),
            LispInteger(2).tag(),
            LispInteger(3).tag(),
        ]);
        let m = super::memq(&LispInteger(2).tag(), &lst).unwrap();
        match m.as_ref() {
            ObjectRef::Cons(c) => {
                let ObjectRef::Int(v) = c.car().as_ref() else {
                    panic!("not int")
                };
                assert_eq!(v, 2);
            }
            _ => panic!("memq not cons"),
        }

        // assoc
        let key_a = Symbol::from("a").tag();
        let key_b = Symbol::from("b").tag();
        let pair_a = cons_from(key_a.clone(), LispInteger(1).tag());
        let pair_b = cons_from(key_b.clone(), LispInteger(2).tag());
        let alist = list_from(vec![pair_a, pair_b.clone()]);
        let found = super::assoc(&key_b, &alist).unwrap();
        match found.as_ref() {
            ObjectRef::Cons(p) => {
                let ObjectRef::Int(v) = p.cdr().as_ref() else {
                    panic!("not int")
                };
                assert_eq!(v, 2);
            }
            _ => panic!("assoc not cons pair"),
        }
    }
}
