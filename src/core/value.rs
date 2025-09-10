use std::{marker::PhantomData, mem::ManuallyDrop, ops::Deref};

use proc_macros::Trace;

use crate::{
    core::{
        cons::{Cons, LispCons},
        function::{Function, LispFunction},
        map::{HashTable, LispHashTable},
        number::{Character, Float, Integer},
        string::LispString,
        symbol::Symbol,
        tagged_ptr::{get_tag, TaggedPtrError},
        vector::Vector,
        TaggedPtr,
    },
    gc::Trace,
};

#[repr(transparent)]
#[derive(PartialEq, PartialOrd, Eq, Hash)]
pub struct Value(pub u64);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl Default for Value {
    fn default() -> Self {
        nil()
    }
}

pub fn nil() -> Value {
    Value(NIL as u64)
}

pub fn tru() -> Value {
    Value(TRUE as u64)
}

unsafe impl Trace for Value {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        // self.untag_ref().trace(visitor);
    }

    unsafe fn finalize(&mut self) {
        todo!()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        let val = Self(self.0);
        let val = val.untag();
        val.clone().tag()
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        let val = Self(self.0);
        let _untagged = val.untag();
        // now untagged should be dropped, causing dec_rc to happen
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
    String,
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
            LispType::String => "string",
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

#[derive(Clone, Debug, Trace)]
pub enum LispValue {
    #[no_trace]
    Nil,
    #[no_trace]
    True,
    #[no_trace]
    Int(Integer),
    #[no_trace]
    Float(Float),
    #[no_trace]
    Character(Character),
    String(LispString),
    Symbol(Symbol),
    Vector(Vector),
    Cons(Cons),
    Function(Function),
    HashTable(HashTable),
}

#[derive(Debug, Clone, Copy)]
pub enum LispValueRef<'a> {
    Nil,
    True,
    Int(i64),
    Float(f64),
    Character(char),
    Symbol(Symbol),
    String(&'a String),
    Vector(&'a Vec<Value>),
    Cons(&'a LispCons),
    Function(&'a LispFunction),
    HashTable(&'a LispHashTable),
}

#[derive(Debug)]
pub enum LispValueMut<'a> {
    Nil,
    True,
    Int(i64),
    Float(f64),
    Character(char),
    Symbol(Symbol),
    String(&'a mut String),
    Vector(&'a mut Vec<Value>),
    Cons(&'a mut LispCons),
    Function(&'a mut LispFunction),
    HashTable(&'a mut LispHashTable),
}

impl Value {
    pub fn untag(self) -> LispValue {
        let tag = self.get_tag();
        match tag {
            LispType::Nil => LispValue::Nil,
            LispType::True => LispValue::True,
            LispType::Int => LispValue::Int(Integer::untag(self).unwrap()),
            LispType::Float => LispValue::Float(Float::untag(self).unwrap()),
            LispType::Character => LispValue::Character(Character::untag(self).unwrap()),
            LispType::String => LispValue::String(LispString::untag(self).unwrap()),
            LispType::Symbol => LispValue::Symbol(Symbol::untag(self).unwrap()),
            LispType::Vector => LispValue::Vector(Vector::untag(self).unwrap()),
            LispType::Cons => LispValue::Cons(Cons::untag(self).unwrap()),
            LispType::Function => LispValue::Function(Function::untag(self).unwrap()),
            LispType::HashTable => LispValue::HashTable(HashTable::untag(self).unwrap()),
        }
    }

    pub fn as_ref(&self) -> LispValueRef<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => LispValueRef::Nil,
                LispType::True => LispValueRef::True,
                LispType::Int => LispValueRef::Int(*Integer::as_ref_unchecked(self)),
                LispType::Float => LispValueRef::Float(*Float::as_ref_unchecked(self)),
                LispType::Character => LispValueRef::Character(*Character::as_ref_unchecked(self)),
                LispType::String => LispValueRef::String(LispString::as_ref_unchecked(self)),
                LispType::Symbol => LispValueRef::Symbol(Symbol::as_ref_unchecked(self).into()),
                LispType::Vector => LispValueRef::Vector(Vector::as_ref_unchecked(self)),
                LispType::Cons => LispValueRef::Cons(Cons::as_ref_unchecked(self)),
                LispType::Function => LispValueRef::Function(Function::as_ref_unchecked(self)),
                LispType::HashTable => LispValueRef::HashTable(HashTable::as_ref_unchecked(self)),
            }
        }
    }

    pub fn as_mut(&mut self) -> LispValueMut<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => LispValueMut::Nil,
                LispType::True => LispValueMut::True,
                LispType::Int => LispValueMut::Int(*Integer::as_mut_unchecked(self)),
                LispType::Float => LispValueMut::Float(*Float::as_mut_unchecked(self)),
                LispType::Character => LispValueMut::Character(*Character::as_mut_unchecked(self)),
                LispType::String => LispValueMut::String(LispString::as_mut_unchecked(self)),
                LispType::Symbol => {
                    let ident = *Symbol::as_mut_unchecked(self);
                    LispValueMut::Symbol(ident.into())
                },
                LispType::Vector => LispValueMut::Vector(Vector::as_mut_unchecked(self)),
                LispType::Cons => LispValueMut::Cons(Cons::as_mut_unchecked(self)),
                LispType::Function => LispValueMut::Function(Function::as_mut_unchecked(self)),
                LispType::HashTable => LispValueMut::HashTable(HashTable::as_mut_unchecked(self)),
            }
        }
    }

    pub fn get_tag(&self) -> LispType {
        get_tag(self.0 as i64)
    }
}

impl LispValue {
    pub fn tag(self) -> Value {
        match self {
            LispValue::Nil => nil(),
            LispValue::True => tru(),
            LispValue::Int(integer) => integer.tag(),
            LispValue::Float(float) => float.tag(),
            LispValue::Character(character) => character.tag(),
            LispValue::String(lisp_string) => lisp_string.tag(),
            LispValue::Symbol(symbol) => symbol.tag(),
            LispValue::Vector(vector) => vector.tag(),
            LispValue::Cons(cons) => cons.tag(),
            LispValue::Function(function) => function.tag(),
            LispValue::HashTable(hash_table) => hash_table.tag(),
        }
    }
}
