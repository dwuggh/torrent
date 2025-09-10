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
        let tag = get_tag(value.0);
        let result = match tag {
            LispType::Nil => LispValue::Nil,
            LispType::True => LispValue::True,
            LispType::Int => LispValue::Int(Integer::untag(value)?),
            _ => todo!()
        };
        todo!()
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
    Cons(&'a mut ConsInner,
    Function(&'a mut FunctionInner),
    Map(&'a mut Map),
}

impl Value {

    pub fn untag(&self) -> LispValue {
        // let tag = self
        let data = self.0;
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => LispValue::Nil,
                LispType::True => LispValue::True,
                LispType::Int => LispValue::Int(*TaggedPtr::untag(data)),
                LispType::Float => LispValue::Float(*TaggedPtr::untag(data)),
                LispType::Character => LispValue::Character(char::from_u32(data as u32).unwrap()),
                LispType::String => LispValue::String(TaggedPtr::untag(data)),
                LispType::Symbol => LispValue::Symbol(*TaggedPtr::untag(data)),
                LispType::Vector => LispValue::Vector(TaggedPtr::untag(data)),
                LispType::Cons => LispValue::Cons(TaggedPtr::untag(data)),
                LispType::Function => LispValue::Function(TaggedPtr::untag(data)),
                LispType::Map => LispValue::Map(TaggedPtr::untag(data)),
            }
        }
    }

    pub fn as_ref(&self) -> LispValueRef<'_> {
        todo!()
    }

    pub fn as_mut(&self) -> LispValueMut<'_> {
        // let tag = self
        let data = self.0;
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => LispValueMut::Nil,
                LispType::True => LispValueMut::True,
                LispType::Int => LispValueMut::Int(*TaggedPtr::as_mut_unchecked(data)),
                LispType::Float => LispValueMut::Float(*TaggedPtr::untag_mut(data)),
                LispType::Character => LispValueMut::Character(char::from_u32(data as u32).unwrap()),
                LispType::String => LispValueMut::String(TaggedPtr::untag_mut(data)),
                LispType::Symbol => LispValueMut::Symbol(*TaggedPtr::untag_mut(data)),
                LispType::Vector => LispValueMut::Vector(TaggedPtr::untag_mut(data)),
                LispType::Cons => LispValueMut::Cons(TaggedPtr::untag_mut(data)),
                LispType::Function => LispValueMut::Function(TaggedPtr::untag_mut(data)),
                LispType::Map => LispValueMut::Map(TaggedPtr::untag_mut(data)),
            }
        }
    }

    pub fn get_tag(&self) -> LispType {
        unsafe { std::mem::transmute(self.0 as u8) }
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

// impl<'a, T: TaggedPtr<'a>> From<T> for Value {
//     fn from(val: T) -> Self {
//         val.tag()
//     }
// }
