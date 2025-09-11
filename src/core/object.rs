use std::{marker::PhantomData, mem::ManuallyDrop, ops::Deref};

use proc_macros::Trace;

use crate::{
    core::{
        cons::{Cons, LispCons},
        function::{Function, LispFunction},
        hashtable::{HashTable, LispHashTable},
        number::{Character, Float, Integer, LispCharacter, LispFloat, LispInteger},
        string::{LispStr, Str},
        symbol::{LispSymbol, Symbol},
        tagged_ptr::{get_tag, TaggedPtrError},
        vector::{LispVector, Vector},
        TaggedPtr,
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

unsafe impl Trace for Object {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        // self.untag_ref().trace(visitor);
    }

    unsafe fn finalize(&mut self) {
        todo!()
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        let val = Self(self.0);
        let val = val.untag();
        val.clone().tag()
    }
}

impl From<LispCons> for Object {
    fn from(cons: LispCons) -> Self {
        cons.tag()
    }
}

impl Drop for Object {
    fn drop(&mut self) {
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
    Symbol(LispSymbol),
    Vector(LispVector),
    Cons(LispCons),
    Function(LispFunction),
    HashTable(LispHashTable),
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
            LispType::Nil => LispObject::Nil,
            LispType::True => LispObject::True,
            LispType::Int => LispObject::Int(LispInteger::untag(self).unwrap()),
            LispType::Float => LispObject::Float(LispFloat::untag(self).unwrap()),
            LispType::Character => LispObject::Character(LispCharacter::untag(self).unwrap()),
            LispType::Str => LispObject::Str(LispStr::untag(self).unwrap()),
            LispType::Symbol => LispObject::Symbol(Symbol::untag(self).unwrap()),
            LispType::Vector => LispObject::Vector(LispVector::untag(self).unwrap()),
            LispType::Cons => LispObject::Cons(LispCons::untag(self).unwrap()),
            LispType::Function => LispObject::Function(LispFunction::untag(self).unwrap()),
            LispType::HashTable => LispObject::HashTable(LispHashTable::untag(self).unwrap()),
        }
    }

    pub fn as_ref(&self) -> ObjectRef<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => ObjectRef::Nil,
                LispType::True => ObjectRef::True,
                LispType::Int => ObjectRef::Int(*LispInteger::as_ref_unchecked(self)),
                LispType::Float => ObjectRef::Float(*LispFloat::as_ref_unchecked(self)),
                LispType::Character => ObjectRef::Character(*LispCharacter::as_ref_unchecked(self)),
                LispType::Str => ObjectRef::Str(LispStr::as_ref_unchecked(self)),
                LispType::Symbol => ObjectRef::Symbol(*Symbol::as_ref_unchecked(self)),
                LispType::Vector => ObjectRef::Vector(LispVector::as_ref_unchecked(self)),
                LispType::Cons => ObjectRef::Cons(LispCons::as_ref_unchecked(self)),
                LispType::Function => ObjectRef::Function(LispFunction::as_ref_unchecked(self)),
                LispType::HashTable => ObjectRef::HashTable(LispHashTable::as_ref_unchecked(self)),
            }
        }
    }

    pub fn as_mut(&mut self) -> ObjectMut<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => ObjectMut::Nil,
                LispType::True => ObjectMut::True,
                LispType::Int => ObjectMut::Int(*LispInteger::as_mut_unchecked(self)),
                LispType::Float => ObjectMut::Float(*LispFloat::as_mut_unchecked(self)),
                LispType::Character => ObjectMut::Character(*LispCharacter::as_mut_unchecked(self)),
                LispType::Str => ObjectMut::Str(LispStr::as_mut_unchecked(self)),
                LispType::Symbol => {
                    let ident = *Symbol::as_mut_unchecked(self);
                    ObjectMut::Symbol(ident.into())
                },
                LispType::Vector => ObjectMut::Vector(LispVector::as_mut_unchecked(self)),
                LispType::Cons => ObjectMut::Cons(LispCons::as_mut_unchecked(self)),
                LispType::Function => ObjectMut::Function(LispFunction::as_mut_unchecked(self)),
                LispType::HashTable => ObjectMut::HashTable(LispHashTable::as_mut_unchecked(self)),
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
        }
    }
}

impl<'a> ObjectRef<'a> {

    pub fn tag(&self) -> LispType {
        match self {
            ObjectRef::Nil => LispType::Nil,
            ObjectRef::True => LispType::True,
            ObjectRef::Int(_) => LispType::Int,
            ObjectRef::Float(_) => LispType::Float,
            ObjectRef::Character(_) => LispType::Character,
            ObjectRef::Symbol(_) => LispType::Symbol,
            ObjectRef::Str(_) => LispType::Str,
            ObjectRef::Vector(_) => LispType::Vector,
            ObjectRef::Cons(_) => LispType::Cons,
            ObjectRef::Function(_) => LispType::Function,
            ObjectRef::HashTable(_) => LispType::HashTable,
        }
    }
}

macro_rules! impl_try_from_for_object {
    ($name:ident, $lispname:ident) => {
        impl<'a> TryFrom<&'a Object> for &'a $name {
            type Error = &'static str;

            fn try_from(value: &'a Object) -> Result<Self, Self::Error> {
                let val = <$lispname as TaggedPtr>::as_ref(&value).unwrap();
                Ok(val)
            }
        }

        impl<'a> TryFrom<&'a Object> for &'a mut $name {
            type Error = &'static str;

            fn try_from(value: &'a Object) -> Result<Self, Self::Error> {
                let val = <$lispname as TaggedPtr>::as_mut(&value).unwrap();
                Ok(val)
            }
        }

    };
}


impl_try_from_for_object!(Str, LispStr);
impl_try_from_for_object!(Symbol, LispSymbol);
impl_try_from_for_object!(Vector, LispVector);
impl_try_from_for_object!(Cons, LispCons);
impl_try_from_for_object!(Function, LispFunction);
impl_try_from_for_object!(HashTable, LispHashTable);