use proc_macros::defun;

use crate::core::object::{Object, ObjectRef, nil, tru};

#[defun(name = "not")]
fn lisp_not(obj: Object) -> Object {
    match obj.as_ref() {
        ObjectRef::Nil => tru(),
        _ => nil(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::number::LispInteger;
    use crate::core::Tag;

    #[test]
    fn test_not() {
        assert!(matches!(lisp_not(crate::core::object::nil()).as_ref(), ObjectRef::True));
        assert!(matches!(lisp_not(LispInteger(1).tag()).as_ref(), ObjectRef::Nil));
    }
}
