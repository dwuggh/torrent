use std::{marker::PhantomData, mem::ManuallyDrop, ops::Deref};

use proc_macros::Trace;

use crate::{
    ast::Node,
    core::{
        compiler::macro_item::MacroItem, cons::{Cons, ConsInner}, function::{Function, FunctionInner}, map::Map, number::{Character, Float, Integer}, string::LispString, symbol::Symbol, tagged_ptr::{get_tag, TaggedPtrError}, vector::Vector, TaggedPtr
    },
    gc::{Gc, GcInner, Trace},
};

#[repr(transparent)]
#[derive(PartialEq, PartialOrd, Eq, Hash)]
pub struct Value(pub u64);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.untag().fmt(f)
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
    Map,
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
            LispType::Map => "hash-table",
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
    Map(Map),
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
            LispType::Map => LispValue::Map(Map::untag(value)?),
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
    String(&'a LispString),
    Symbol(Symbol),
    Vector(&'a Vector),
    Cons(&'a Cons),
    Function(&'a Function),
    Map(&'a Map),
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
    Cons(&'a mut ConsInner),
    Function(&'a mut FunctionInner),
    Map(&'a mut Map),
}

impl Value {

    pub fn untag(&self) -> LispValue {
        let tag = self.get_tag();
        match tag {
            LispType::Nil => LispValue::Nil,
            LispType::True => LispValue::True,
            LispType::Int => LispValue::Int(Integer::untag(*self).unwrap()),
            LispType::Float => LispValue::Float(Float::untag(*self).unwrap()),
            LispType::Character => LispValue::Character(Character::untag(*self).unwrap()),
            LispType::String => LispValue::String(LispString::untag(*self).unwrap()),
            LispType::Symbol => LispValue::Symbol(Symbol::untag(*self).unwrap()),
            LispType::Vector => LispValue::Vector(Vector::untag(*self).unwrap()),
            LispType::Cons => LispValue::Cons(Cons::untag(*self).unwrap()),
            LispType::Function => LispValue::Function(Function::untag(*self).unwrap()),
            LispType::Map => LispValue::Map(Map::untag(*self).unwrap()),
        }
    }

    pub fn as_ref(&self) -> LispValueRef<'_> {
        let tag = self.get_tag();
        match tag {
            LispType::Nil => LispValueRef::Nil,
            LispType::True => LispValueRef::True,
            LispType::Int => LispValueRef::Int(Integer::untag(*self).unwrap().value()),
            LispType::Float => LispValueRef::Float(Float::untag(*self).unwrap().value()),
            LispType::Character => LispValueRef::Character(Character::untag(*self).unwrap().value()),
            LispType::String => LispValueRef::String(&LispString::untag(*self).unwrap()),
            LispType::Symbol => LispValueRef::Symbol(Symbol::untag(*self).unwrap()),
            LispType::Vector => LispValueRef::Vector(&Vector::untag(*self).unwrap()),
            LispType::Cons => LispValueRef::Cons(&Cons::untag(*self).unwrap()),
            LispType::Function => LispValueRef::Function(&Function::untag(*self).unwrap()),
            LispType::Map => LispValueRef::Map(&Map::untag(*self).unwrap()),
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
                    function.inner.inc_ref_count();
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

macro_rules! impl_try_from_value_variant_copy {
    ($($ty:ty => $variant:ident),+ $(,)?) => {
        $(
            impl ::std::convert::TryFrom<&Value> for $ty {
                type Error = &'static str;
                fn try_from(value: &Value) -> ::std::result::Result<Self, Self::Error> {
                    match value.untag() {
                        LispValue::$variant(inner) => Ok(inner),
                        _ => Err(concat!("expected ", stringify!($variant))),
                    }
                }
            }
        )+
    };
}

macro_rules! impl_try_from_value_variant_ref {
    ($($ty:ty => $variant:ident),+ $(,)?) => {
        $(
            impl<'a> ::std::convert::TryFrom<&'a Value> for $ty {
                type Error = &'static str;
                fn try_from(value: &'a Value) -> ::std::result::Result<Self, Self::Error> {
                    match value.untag() {
                        LispValue::$variant(inner) => Ok(inner),
                        _ => Err(concat!("expected ", stringify!($variant))),
                    }
                }
            }
        )+
    };
}

// Implement TryFrom<Value> for copy types (returned by value)
impl_try_from_value_variant_copy! {
    i64 => Int,
    f64 => Float,
    char => Character,
    Symbol => Symbol,
}

// Implement TryFrom<Value> for reference types (returned by reference)
impl_try_from_value_variant_ref! {
    &'a LispString => String,
    &'a Vector => Vector,
    &'a Cons => Cons,
    &'a Function => Function,
    &'a Map => Map,
}

impl From<Integer> for Value {
    fn from(val: Integer) -> Self {
        val.tag()
    }
}

impl From<Float> for Value {
    fn from(val: Float) -> Self {
        val.tag()
    }
}

impl From<Character> for Value {
    fn from(val: Character) -> Self {
        val.tag()
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Integer::new(val).tag()
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Float::new(val).tag()
    }
}

impl From<char> for Value {
    fn from(val: char) -> Self {
        Character::new(val).tag()
    }
}
