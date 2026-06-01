use proc_macros::defun;

use crate::core::object::{Object, ObjectRef, nil, tru};

#[defun]
fn eq(a: Object, b: Object) -> Object {
    if a.0 == b.0 { tru() } else { nil() }
}

fn equal_deep(a: &Object, b: &Object) -> bool {
    use crate::core::object::ObjectRef::*;
    match (a.as_ref(), b.as_ref()) {
        (Nil, Nil) | (True, True) => true,
        (Int(x), Int(y)) => x == y,
        (Float(x), Float(y)) => x == y,
        (Character(x), Character(y)) => x == y,
        (Symbol(x), Symbol(y)) => x == y,
        (Str(x), Str(y)) => x == y,
        (Vector(xs), Vector(ys)) => {
            if xs.len() != ys.len() { return false; }
            for (vx, vy) in xs.iter().zip(ys.iter()) {
                if !equal_deep(vx, vy) { return false; }
            }
            true
        }
        (Cons(cx), Cons(cy)) => equal_deep(cx.car(), cy.car()) && equal_deep(cx.cdr(), cy.cdr()),
        _ => a.0 == b.0,
    }
}

#[defun]
fn equal(a: Object, b: Object) -> Object {
    if equal_deep(&a, &b) { tru() } else { nil() }
}

#[defun]
fn null(obj: Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Nil => tru(),
        _ => nil(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::number::LispInteger;
    use crate::core::cons::LispCons;
    use crate::core::Tag;

    #[test]
    fn test_eq_basic() {
        assert!(matches!(eq(LispInteger(1).tag(), LispInteger(1).tag()).as_ref(), ObjectRef::True));
        assert!(matches!(eq(LispInteger(1).tag(), LispInteger(2).tag()).as_ref(), ObjectRef::Nil));
    }

    #[test]
    fn test_equal_lists() {
        let l1 = LispCons::from_iter(vec![LispInteger(1).tag(), LispInteger(2).tag()].into_iter()).unwrap().tag();
        let l2 = LispCons::from_iter(vec![LispInteger(1).tag(), LispInteger(2).tag()].into_iter()).unwrap().tag();
        assert!(matches!(equal(l1.clone(), l2.clone()).as_ref(), ObjectRef::True));
    }

    #[test]
    fn test_null() {
        assert!(matches!(null(nil()).as_ref(), ObjectRef::True));
    }
}
