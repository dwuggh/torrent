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
        Self::from_raw_inc_rc(self.0)
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

#[derive(Clone, Debug)]
pub enum LispValue {
    Nil,
    True,
    Int(Integer),
    Float(Float),
    Character(Character),
    String(LispString),
    Symbol(Symbol),
    Vector(Vector),
    Cons(Cons),
    Function(Function),
    Map(HashTable),
}

impl TryFrom<Value> for LispValue {
    type Error = TaggedPtrError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let tag = get_tag(value.0 as i64);
        let result = match tag {
            LispType::Nil => LispValue::Nil,
            LispType::True => LispValue::True,
            LispType::Int => LispValue::Int(Integer::untag(value)?),
            LispType::Float => LispValue::Float(Float::untag(value)?),
            LispType::Character => LispValue::Character(Character::untag(value)?),
            LispType::String => LispValue::String(LispString::untag(value)?),
            LispType::Symbol => LispValue::Symbol(Symbol::untag(value)?),
            LispType::Vector => LispValue::Vector(Vector::untag(value)?),
            LispType::Cons => LispValue::Cons(Cons::untag(value)?),
            LispType::Function => LispValue::Function(Function::untag(value)?),
            LispType::HashTable => LispValue::Map(HashTable::untag(value)?),
        };
        Ok(result)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LispValueRef<'a> {
    Nil,
    True,
    Int(i64),
    Float(f64),
    Character(char),
    String(&'a String),
    Symbol(Symbol),
    Vector(&'a Vec<Value>),
    Cons(&'a LispCons),
    Function(&'a LispFunction),
    HashTable(&'a LispHashTable),
}

#[derive(Debug, Trace)]
pub enum LispValueMut<'a> {
    #[no_trace]
    Nil,
    #[no_trace]
    True,
    #[no_trace]
    Int(i64),
    #[no_trace]
    Float(f64),
    #[no_trace]
    Character(char),
    #[no_trace]
    String(&'a mut String),
    #[no_trace]
    Symbol(Symbol),
    Vector(&'a mut Vec<Value>),
    Cons(&'a mut LispCons),
    Function(&'a mut LispFunction),
    Map(&'a mut HashTable),
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
            LispType::HashTable => LispValue::Map(HashTable::untag(self).unwrap()),
        }
    }

    pub fn as_ref(&self) -> LispValueRef<'_> {
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => LispValueRef::Nil,
                LispType::True => LispValueRef::True,
                LispType::Int => LispValueRef::Int(Integer::untag(*self).unwrap().value()),
                LispType::Float => LispValueRef::Float(Float::untag(*self).unwrap().value()),
                LispType::Character => {
                    LispValueRef::Character(Character::untag(*self).unwrap().value())
                }

                LispType::String => LispValueRef::String(LispString::as_ref_unchecked(self)),
                LispType::Symbol => LispValueRef::Symbol(Symbol::untag(*self).unwrap()),
                LispType::Vector => LispValueRef::Vector(&Vector::untag(*self).unwrap()),
                LispType::Cons => LispValueRef::Cons(&Cons::untag(*self).unwrap()),
                LispType::Function => LispValueRef::Function(&Function::untag(*self).unwrap()),
                LispType::HashTable => LispValueRef::Map(&HashTable::untag(*self).unwrap()),
            }
        }
    }

    pub fn get_tag(&self) -> LispType {
        get_tag(self.0 as i64)
    }

    pub fn from_raw_inc_rc(raw: u64) -> Self {
        let val = Value(raw);
        let untagged = val.untag();
        unsafe {
            match untagged {
                LispValue::String(string) => string.inc_strong_rc(),
                // as long as the obarray is traced, its traced
                LispValue::Symbol(_) => {}
                LispValue::Vector(vector) => {
                    vector.0.inc_ref_count();
                }
                LispValue::Cons(cons) => {
                    cons.0.inc_ref_count();
                }
                LispValue::Function(function) => {
                    function.0.inc_ref_count();
                }
                LispValue::Map(map) => {
                    map.0.inc_ref_count();
                }
                _ => (),
            }
        }
        val
    }
}
