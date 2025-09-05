use std::{marker::PhantomData, mem::ManuallyDrop, ops::Deref, sync::Arc};

use proc_macros::{Trace, defun};

use crate::{core::{function::Function, string::LispString, symbol::Symbol}, gc::{Gc, GcInner, Trace}};

#[repr(transparent)]
#[derive(PartialEq, PartialOrd, Eq, Copy, Debug)]
pub struct Value(pub u64);

unsafe impl Trace for Value {
    unsafe fn trace(&self, visitor: crate::gc::Visitor) {
        self.untag_ref().trace(visitor);
    }

    unsafe fn finalize(&mut self) {
        ManuallyDrop::new(self.untag()).finalize();
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        Self::from_raw_inc_rc(self.0)
    }
}

#[repr(u8)]
pub enum LispType {
    Int = 0,
    Nil,
    Float,
    Character,
    String,
    Symbol,
    Vector,
    Cons,
    Function,
}

pub const NIL: i64 = LispType::Nil as i64;

#[derive(Clone, Trace, Debug)]
pub enum LispValue {
    #[no_trace]
    Nil,
    #[no_trace]
    Int(i64),
    #[no_trace]
    Float(f64),
    #[no_trace]
    Character(char),
    #[no_trace]
    String(LispString),
    Symbol(Symbol),
    Vector(Vector),
    Cons(Cons),
    Function(Function)
}

impl Value {
    pub fn untag(self) -> LispValue {
        // let tag = self
        let data = self.0 >> 8;
        let tag = self.get_tag();
        unsafe {
            match tag {
                LispType::Nil => LispValue::Nil,
                LispType::Int => LispValue::Int(TaggedPtr::cast(data)),
                LispType::Float => LispValue::Float(TaggedPtr::cast(data)),
                LispType::Character => LispValue::Character(char::from_u32(data as u32).unwrap()),
                LispType::String => LispValue::String(TaggedPtr::cast(data)),
                LispType::Symbol => LispValue::Symbol(TaggedPtr::cast(data)),
                LispType::Vector => LispValue::Vector(TaggedPtr::cast(data)),
                LispType::Cons => LispValue::Cons(TaggedPtr::cast(data)),
                LispType::Function => LispValue::Function(TaggedPtr::cast(data)),
            }
        }
    }

    pub fn untag_ref(&self) -> LispValueRef<'_> {
        LispValueRef {untagged: ManuallyDrop::new(self.untag()), phantom: PhantomData}
    }

    pub fn get_tag(self) -> LispType {
        unsafe { std::mem::transmute(self.0 as u8) }
    }

    pub fn tag<T: TaggedPtr>(val: T) -> Self {
        TaggedPtr::tag(val)
    }

    pub fn from_raw_inc_rc(raw: u64) -> Self {
        let val = Value(raw);
        let untagged = val.untag();
        unsafe {
            match untagged {
                LispValue::String(string) => string.inc_strong_rc(),
                LispValue::Symbol(symbol) => {
                    if let Some(cell) = symbol.get() {
                        cell.value().0.inc_ref_count();
                    }
                }
                LispValue::Vector(vector) => {
                    vector.0.inc_ref_count();
                }
                LispValue::Cons(cons) => {
                    cons.0.inc_ref_count();
                }
                LispValue::Function(function) => {
                    function.inner.inc_ref_count();
 }
                _ => ()
            }
        }
        val
    }

}

impl Deref for LispValueRef<'_> {
    type Target = LispValue;

    fn deref(&self) -> &Self::Target {
        &self.untagged
    }
}

pub struct LispValueRef<'a> {
    untagged: ManuallyDrop<LispValue>,
    phantom: PhantomData<&'a LispValue>,
}

pub trait TaggedPtr: Sized {
    const TAG: LispType;

    unsafe fn cast(val: u64) -> Self;

    /// Given the type, return a tagged version of it.
    fn tag(self) -> Value {
        unsafe {
            Value(self.get_untagged_data() << 8 | Self::TAG as u64)
        }
    }

    unsafe fn get_untagged_data(self) -> u64;
}

pub(crate) const MAX_FIXNUM: i64 = i64::MAX >> 8;
pub(crate) const MIN_FIXNUM: i64 = i64::MIN >> 8;
impl TaggedPtr for i64 {
    const TAG: LispType = LispType::Int;
    unsafe fn cast(val: u64) -> Self {
        val as i64
    }

    unsafe fn get_untagged_data(self) -> u64 {
        self.clamp(MIN_FIXNUM, MAX_FIXNUM) as u64
    }
}

impl TaggedPtr for f64 {
    const TAG: LispType = LispType::Float;
    unsafe fn cast(val: u64) -> Self {
        val as f64
    }

    // TODO
    unsafe fn get_untagged_data(self) -> u64 {
        self as u64
    }
}

impl TaggedPtr for Cons {
    const TAG: LispType = LispType::Cons;

    unsafe fn cast(val: u64) -> Self {
        Cons(Gc::from_raw(val as *mut GcInner<ConsInner>))
    }

    unsafe fn get_untagged_data(self) -> u64 {
        Gc::into_raw(self.0) as u64
    }
}

impl TaggedPtr for Vector {
    const TAG: LispType = LispType::Vector;

    unsafe fn cast(val: u64) -> Self {
        Vector(Gc::from_raw(val as *mut GcInner<Vec<Value>>))
    }

    unsafe fn get_untagged_data(self) -> u64 {
        Gc::into_raw(self.0) as u64
    }
}

#[derive(Clone, Trace, Debug)]
pub struct Cons(Gc<ConsInner>);

#[derive(Clone, Trace, Debug)]
pub struct ConsInner {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Trace, Debug)]
pub struct Vector(Gc<Vec<Value>>);

macro_rules! impl_try_from_value_variant {
    ($($ty:ty => $variant:ident),+ $(,)?) => {
        $(
            impl ::std::convert::TryFrom<Value> for $ty {
                type Error = &'static str;
                fn try_from(value: Value) -> ::std::result::Result<Self, Self::Error> {
                    match value.untag() {
                        LispValue::$variant(inner) => Ok(inner),
                        _ => Err(concat!("expected ", stringify!($variant))),
                    }
                }
            }
        )+
    };
}

// Implement TryFrom<Value> for each inner LispValue variant type.
impl_try_from_value_variant! {
    i64 => Int,
    f64 => Float,
    char => Character,
    LispString => String,
    Symbol => Symbol,
    Vector => Vector,
    Cons => Cons,
    Function => Function,
}

// Useful blanket: convert Value -> LispValue via untag (infallible).
impl ::std::convert::TryFrom<Value> for LispValue {
    type Error = ::std::convert::Infallible;
    fn try_from(value: Value) -> ::std::result::Result<Self, Self::Error> {
        Ok(value.untag())
    }
}

// Optional convenience: treat Nil as unit type.
impl ::std::convert::TryFrom<Value> for () {
    type Error = &'static str;
    fn try_from(value: Value) -> ::std::result::Result<Self, Self::Error> {
        match value.untag() {
            LispValue::Nil => Ok(()),
            _ => Err("expected Nil"),
        }
    }
}
