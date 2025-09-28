use proc_macros::Trace;

use crate::{
    core::{
        cons::{Cons, LispCons},
        function::{Function, LispFunction},
        hashtable::{HashTable, LispHashTable},
        indirect::Indirect,
        number::{Character, Float, Integer, LispCharacter, LispFloat, LispInteger},
        string::{LispStr, Str},
        symbol::{LispSymbol, Symbol},
        tagged_ptr::{TaggedObj, get_tag},
        vector::{LispVector, Vector},
    },
    gc::Trace,
};

#[repr(transparent)]
#[derive(PartialEq, PartialOrd, Eq, Hash)]
pub struct Object(pub u64);

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl Default for Object {
    fn default() -> Self {
        nil()
    }
}

pub fn nil() -> Object {
    Object(NIL as u64)
}

pub fn tru() -> Object {
    Object(TRUE as u64)
}

impl Object {
    pub fn inc_rc(&self) {
        let obj = self.clone();
        std::mem::forget(obj);
    }

    pub fn is_primitive(&self) -> bool {
        match self.get_tag() {
            LispType::Int
            | LispType::Nil
            | LispType::True
            | LispType::Float
            | LispType::Character
            | LispType::Symbol => true,
            _ => false,
        }
    }
}

unsafe impl Trace for Object {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        unsafe {
            match self.as_ref() {
                ObjectRef::Str(str) => str.trace(visitor),
                ObjectRef::Vector(vector) => vector.trace(visitor),
                ObjectRef::Cons(cons) => cons.trace(visitor),
                ObjectRef::Function(function) => function.trace(visitor),
                ObjectRef::HashTable(hash_table) => hash_table.trace(visitor),
                _ => {}
            }
            // self.untag_ref().trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        let new_this = Object(self.0);
        new_this.untag();
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        let obj = Self(self.0);
        let obj = obj.untag();
        let result = obj.clone().tag();
        std::mem::forget(obj);
        result
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        tracing::trace!("calling drop: {self:?}");
        let new_this = Object(self.0);
        new_this.untag();
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LispType {
    Int = 0,
    Nil,
    True,
    Float,
    Character,
    Str,
    Symbol,
    Vector,
    Cons,
    Function,
    HashTable,

    Indirect,
}

impl std::fmt::Display for LispType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_name = match self {
            LispType::Int => "integer",
            LispType::Nil => "nil",
            LispType::True => "boolean",
            LispType::Float => "float",
            LispType::Character => "character",
            LispType::Str => "string",
            LispType::Symbol => "symbol",
            LispType::Vector => "vector",
            LispType::Cons => "cons",
            LispType::Function => "function",
            LispType::HashTable => "hash-table",
            LispType::Indirect => "Indirect",
        };
        write!(f, "{}", type_name)
    }
}

pub const NIL: i64 = LispType::Nil as i64;
pub const TRUE: i64 = LispType::True as i64;

#[derive(Clone, Debug, Trace, Default)]
pub enum LispObject {
    #[default]
    #[no_trace]
    Nil,
    #[no_trace]
    True,
    #[no_trace]
    Int(LispInteger),
    #[no_trace]
    Float(LispFloat),
    #[no_trace]
    Character(LispCharacter),
    Str(LispStr),
    #[no_trace]
    Symbol(LispSymbol),
    Vector(LispVector),
    Cons(LispCons),
    Function(LispFunction),
    HashTable(LispHashTable),
    Indirect(Indirect),
}

#[derive(Debug, Clone, Copy, Default)]
pub enum ObjectRef<'a> {
    #[default]
    Nil,
    True,
    Int(Integer),
    Float(Float),
    Character(Character),
    Symbol(Symbol),
    Str(&'a Str),
    Vector(&'a Vec<Object>),
    Cons(&'a Cons),
    Function(&'a Function),
    HashTable(&'a HashTable),
}

#[derive(Debug, Default)]
pub enum ObjectMut<'a> {
    #[default]
    Nil,
    True,
    Int(Integer),
    Float(Float),
    Character(Character),
    Symbol(Symbol),
    Str(&'a mut Str),
    Vector(&'a mut Vector),
    Cons(&'a mut Cons),
    Function(&'a mut Function),
    HashTable(&'a mut HashTable),
}

impl Object {
    pub fn untag(self) -> LispObject {
        let tag = self.get_tag();
        match tag {
            LispType::Nil => {
                std::mem::forget(self);
                LispObject::Nil
            }
            LispType::True => {
                std::mem::forget(self);
                LispObject::True
            }
            LispType::Int => LispObject::Int(LispInteger::untag(self).unwrap()),
            LispType::Float => LispObject::Float(LispFloat::untag(self).unwrap()),
            LispType::Character => LispObject::Character(LispCharacter::untag(self).unwrap()),
            LispType::Str => LispObject::Str(LispStr::untag(self).unwrap()),
            LispType::Symbol => LispObject::Symbol(LispSymbol::untag(self).unwrap()),
            LispType::Vector => LispObject::Vector(LispVector::untag(self).unwrap()),
            LispType::Cons => LispObject::Cons(LispCons::untag(self).unwrap()),
            LispType::Function => LispObject::Function(LispFunction::untag(self).unwrap()),
            LispType::HashTable => LispObject::HashTable(LispHashTable::untag(self).unwrap()),
            LispType::Indirect => LispObject::Indirect(Indirect::untag(self).unwrap()),
        }
    }

    pub fn as_ref(&self) -> ObjectRef<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => ObjectRef::Nil,
                LispType::True => ObjectRef::True,
                LispType::Int => ObjectRef::Int(self.untagged_as_ref_unchecked::<LispInteger>()),
                LispType::Float => ObjectRef::Float(self.untagged_as_ref_unchecked::<LispFloat>()),
                LispType::Character => {
                    ObjectRef::Character(self.untagged_as_ref_unchecked::<LispCharacter>())
                }
                LispType::Symbol => {
                    ObjectRef::Symbol(self.untagged_as_ref_unchecked::<LispSymbol>())
                }
                LispType::Str => ObjectRef::Str(self.untagged_as_ref_unchecked::<LispStr>()),
                LispType::Vector => {
                    ObjectRef::Vector(self.untagged_as_ref_unchecked::<LispVector>())
                }
                LispType::Cons => ObjectRef::Cons(self.untagged_as_ref_unchecked::<LispCons>()),
                LispType::Function => {
                    ObjectRef::Function(self.untagged_as_ref_unchecked::<LispFunction>())
                }
                LispType::HashTable => {
                    ObjectRef::HashTable(self.untagged_as_ref_unchecked::<LispHashTable>())
                }
                LispType::Indirect => {
                    let inner_obj = self.untagged_as_ref_unchecked::<Indirect>();
                    inner_obj.as_ref()
                }
            }
        }
    }

    pub fn as_mut(&mut self) -> ObjectMut<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => ObjectMut::Nil,
                LispType::True => ObjectMut::True,
                LispType::Int => ObjectMut::Int(self.untagged_as_mut_unchecked::<LispInteger>()),
                LispType::Float => ObjectMut::Float(self.untagged_as_mut_unchecked::<LispFloat>()),
                LispType::Character => {
                    ObjectMut::Character(self.untagged_as_mut_unchecked::<LispCharacter>())
                }
                LispType::Symbol => {
                    ObjectMut::Symbol(self.untagged_as_mut_unchecked::<LispSymbol>())
                }
                LispType::Str => ObjectMut::Str(self.untagged_as_mut_unchecked::<LispStr>()),
                LispType::Vector => {
                    ObjectMut::Vector(self.untagged_as_mut_unchecked::<LispVector>())
                }
                LispType::Cons => ObjectMut::Cons(self.untagged_as_mut_unchecked::<LispCons>()),
                LispType::Function => {
                    ObjectMut::Function(self.untagged_as_mut_unchecked::<LispFunction>())
                }
                LispType::HashTable => {
                    ObjectMut::HashTable(self.untagged_as_mut_unchecked::<LispHashTable>())
                }
                LispType::Indirect => {
                    let inner_obj = self.untagged_as_mut_unchecked::<Indirect>();
                    inner_obj.as_mut()
                }
            }
        }
    }

    pub fn get_tag(&self) -> LispType {
        get_tag(self.0 as i64)
    }
}

impl LispObject {
    pub fn tag(self) -> Object {
        match self {
            LispObject::Nil => nil(),
            LispObject::True => tru(),
            LispObject::Int(integer) => integer.tag(),
            LispObject::Float(float) => float.tag(),
            LispObject::Character(character) => character.tag(),
            LispObject::Str(lisp_string) => lisp_string.tag(),
            LispObject::Symbol(symbol) => symbol.tag(),
            LispObject::Vector(vector) => vector.tag(),
            LispObject::Cons(cons) => cons.tag(),
            LispObject::Function(function) => function.tag(),
            LispObject::HashTable(hash_table) => hash_table.tag(),
            LispObject::Indirect(indirect) => indirect.tag(),
        }
    }
}

// impl<'a> ObjectRef<'a> {
//     pub fn tag(&self) -> LispType {
//         match self {
//             ObjectRef::Nil => LispType::Nil,
//             ObjectRef::True => LispType::True,
//             ObjectRef::Int(_) => LispType::Int,
//             ObjectRef::Float(_) => LispType::Float,
//             ObjectRef::Character(_) => LispType::Character,
//             ObjectRef::Symbol(_) => LispType::Symbol,
//             ObjectRef::Str(_) => LispType::Str,
//             ObjectRef::Vector(_) => LispType::Vector,
//             ObjectRef::Cons(_) => LispType::Cons,
//             ObjectRef::Function(_) => LispType::Function,
//             ObjectRef::HashTable(_) => LispType::HashTable,
//         }
//     }
// }

macro_rules! impl_try_from_for_object {
    ($name:ident, $lispname:ident) => {
        impl<'a> TryFrom<&'a Object> for &'a $name {
            type Error = &'static str;

            fn try_from(object: &'a Object) -> Result<Self, Self::Error> {
                tracing::debug!("in try_into: {object:?}, {}", object.0);
                match object.untagged_as_ref::<$lispname>() {
                    Some(obj) => Ok(obj),
                    None => Err("wrong type"),
                }
            }
        }

        impl<'a> TryFrom<&'a Object> for &'a mut $name {
            type Error = &'static str;

            fn try_from(object: &'a Object) -> Result<Self, Self::Error> {
                match object.untagged_as_mut::<$lispname>() {
                    Some(obj) => Ok(obj),
                    None => Err("wrong type"),
                }
            }
        }
    };
}

macro_rules! impl_try_from_for_primitive {
    ($name:ident, $lispname:ident) => {
        impl TryFrom<&Object> for $name {
            type Error = &'static str;

            fn try_from(object: &Object) -> Result<Self, Self::Error> {
                tracing::debug!("try from: {:.x}", object.0);
                match object.untagged_as_ref::<$lispname>() {
                    Some(obj) => Ok(obj),
                    None => Err("wrong type"),
                }
            }
        }
        impl TryFrom<&mut Object> for $name {
            type Error = &'static str;

            fn try_from(object: &mut Object) -> Result<Self, Self::Error> {
                match object.untagged_as_ref::<$lispname>() {
                    Some(obj) => Ok(obj),
                    None => Err("wrong type"),
                }
            }
        }
    };
}

impl_try_from_for_primitive!(Integer, LispInteger);
impl_try_from_for_primitive!(Float, LispFloat);
impl_try_from_for_primitive!(Character, LispCharacter);
impl_try_from_for_primitive!(Symbol, LispSymbol);

impl_try_from_for_object!(Str, LispStr);
impl_try_from_for_object!(Vector, LispVector);
impl_try_from_for_object!(Cons, LispCons);
impl_try_from_for_object!(Function, LispFunction);
impl_try_from_for_object!(HashTable, LispHashTable);
