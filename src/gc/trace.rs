use std::{collections::HashMap, marker::PhantomData, mem::ManuallyDrop, ptr::drop_in_place};

use crate::gc::{Gc, OpaqueGcPtr};

pub type Visitor = unsafe fn(OpaqueGcPtr);

pub unsafe trait Trace {
    unsafe fn trace(&self, visitor: Visitor);
    unsafe fn finalize(&mut self) {
        drop_in_place(self as *mut Self);
    }
}

macro_rules! impl_empty_trace {
    ( $( $x:ty ),* ) => {
        $(
            unsafe impl Trace for $x {
                unsafe fn trace(&self, _visitor: Visitor) {}
            }
        )*
    }
}

impl_empty_trace! {
    (),
    bool,
    char,
    f32,
    f64,
    // fn
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    // pointer
    // reference
    // slice,
    // tuple
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    &'static str,
    String
}

// Function pointer impls:
unsafe impl<A, B> Trace for fn(A) -> B
where
    A: ?Sized + 'static,
    B: ?Sized + 'static,
{
    unsafe fn trace(&self, _visitor: Visitor) {}
}

unsafe impl<T> Trace for std::sync::Arc<T>
where
    T: ?Sized + 'static,
{
    unsafe fn trace(&self, _visitor: Visitor) {
        // We cannot visit the children for an Arc, as it may lead to situations
        // were we incorrectly decrement a child twice.
        // An Arc wrapping a Gc effectively creates an additional ref count for
        // that Gc that we cannot access.
    }
}

unsafe impl<T: ?Sized + 'static> Trace for Gc<T> {
    unsafe fn trace(&self, visitor: Visitor) {
        visitor(self.as_opaque());
    }

    unsafe fn finalize(&mut self) {}
}

unsafe impl<T: Trace> Trace for Option<T> {
    unsafe fn trace(&self, visitor: Visitor) {
        unsafe {
            if let Some(inner) = self {
                inner.trace(visitor)
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            if let Some(inner) = self {
                inner.finalize();
            }
        }
    }
}

unsafe impl<T> Trace for Vec<T>
where
    T: Trace,
{
    unsafe fn trace(&self, visitor: Visitor) {
        unsafe {
            for child in self {
                child.trace(visitor);
            }
        }
    }

    unsafe fn finalize(&mut self) {
        unsafe {
            for mut child in std::mem::take(self).into_iter().map(ManuallyDrop::new) {
                child.finalize();
            }
        }
    }
}

unsafe impl<K, V> Trace for std::collections::HashMap<K, V>
where
    K: Trace,
    V: Trace,
{
    unsafe fn trace(&self, visitor: Visitor) {
        for (k, v) in self.iter() {
            k.trace(visitor);
            v.trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for (mut k, mut v) in std::mem::take(self)
            .into_iter()
            .map(|(k, v)| (ManuallyDrop::new(k), ManuallyDrop::new(v)))
        {
            k.finalize();
            v.finalize();
        }
    }
}

unsafe impl<T> Trace for PhantomData<T> {
    unsafe fn trace(&self, _: Visitor) {}

    unsafe fn finalize(&mut self) {}
}

unsafe impl<K, V> Trace for rustc_hash::FxHashMap<K, V>
where
    V: Trace,
{
    unsafe fn trace(&self, visitor: Visitor) {
        for (_, v) in self.iter() {
            v.trace(visitor);
        }
    }

    unsafe fn finalize(&mut self) {
        for (_, mut v) in std::mem::take(self)
            .into_iter()
            .map(|(k, v)| (ManuallyDrop::new(k), ManuallyDrop::new(v)))
        {
            v.finalize();
        }
    }
}
