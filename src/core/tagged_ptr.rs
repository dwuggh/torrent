use std::mem::ManuallyDrop;
use thiserror::Error;

use crate::{
    core::object::{LispType, Object},
    gc::{Gc, GcInner},
};

pub fn get_tag(val: i64) -> LispType {
    unsafe { std::mem::transmute(val as u8) }
}
pub fn shifting_tag(val: u64, tag: LispType) -> u64 {
    val << 8 | tag as u64
}
pub fn shifting_untag(val: u64) -> u64 {
    val >> 8
}

#[derive(Error, Debug, Clone)]
pub enum TaggedPtrError {
    #[error("encounted null pointer")]
    NullPointer,
    #[error("type mismatch")]
    TypeMisMatch,
}

pub trait TaggedObj: Tagged {
    fn untag(val: Object) -> Result<Self, TaggedPtrError> {
        if get_tag(val.0 as i64) == Self::TAG {
            let val = ManuallyDrop::new(val);
            Ok(unsafe { Self::from_raw(val.0) })
            // let ptr = Self::untag_ptr(val.0);
            // ptr.try_into().map_err(|_| TaggedPtrError::NullPointer)
        } else {
            Err(TaggedPtrError::TypeMisMatch)
        }
    }

    fn tag(self) -> Object {
        let this = ManuallyDrop::new(self);
        Object(unsafe { this.to_raw() })
    }
}

impl<T: Tagged> TaggedObj for T {}

pub trait Tagged: Sized {
    const TAG: LispType;
    type Data<'a>: Copy;
    type DataMut<'a>;

    /// get the tagged value for `Self`
    unsafe fn to_raw(&self) -> u64 {
        unimplemented!()
    }

    /// restore `Self` from the tagged value
    unsafe fn from_raw(raw: u64) -> Self {
        unimplemented!()
    }


    unsafe fn cast<'a>(val: u64) -> Self::Data<'a>;
    unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a>;
}

pub(crate) const MAX_FIXNUM: i64 = i64::MAX >> 8;
pub(crate) const MIN_FIXNUM: i64 = i64::MIN >> 8;

macro_rules! impl_tagged_for_gc {
    ($name:ident, $lispty:expr, $inner:ty) => {
        impl From<crate::gc::Gc<$inner>> for $name {
            fn from(value: crate::gc::Gc<$inner>) -> Self {
                Self(value)
            }
        }

        impl Tagged for $name {
            const TAG: LispType = $lispty;
            type Data<'a> = &'a $inner;
            type DataMut<'a> = &'a mut $inner;

            unsafe fn to_raw(&self) -> u64 {
                let val = self.0.to_raw() as u64;
                val << 8 | Self::TAG as u64
            }

            unsafe fn from_raw(raw: u64) -> Self {
                // std::mem::transmute(raw)
                let val = crate::core::tagged_ptr::shifting_untag(raw);
                let ptr = val as *mut crate::gc::GcInner<$inner>;
                let ptr = std::ptr::NonNull::new(ptr).unwrap();
                let val = crate::gc::Gc::new_raw(ptr);
                Self(val)
                // Ok(val)
            }

            unsafe fn cast<'a>(val: u64) -> Self::Data<'a> {
                let val = crate::core::tagged_ptr::shifting_untag(val);
                let ptr = val as *mut crate::gc::GcInner<$inner>;
                let data_ref = ptr.as_ref().map(AsRef::as_ref).unwrap();
                std::mem::transmute(data_ref)
            }

            unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a> {
                let val = crate::core::tagged_ptr::shifting_untag(val);
                let ptr = val as *mut crate::gc::GcInner<$inner>;
                let data_ref = ptr.as_mut().map(AsMut::as_mut).unwrap();
                std::mem::transmute(data_ref)
            }
        }
    };
}

macro_rules! impl_tagged_for_prim {
    ($name:ident, $lispty:expr, $inner:ty) => {
        impl Tagged for $name {
            const TAG: LispType = $lispty;
            type Data<'a> = $inner;
            type DataMut<'a> = $inner;

            unsafe fn to_raw(&self) -> u64 {
                shifting_tag(self.0 as u64, Self::TAG)
            }

            unsafe fn from_raw(raw: u64) -> Self {
                let val = shifting_untag(raw);
                std::mem::transmute(val)
            }

            unsafe fn cast<'a>(val: u64) -> Self::Data<'a> {
                Self::from_raw(val).0
            }

            unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a> {
                Self::from_raw(val).0
            }
        }
    };
}


impl Object {
    pub fn untagged_as_ref<T: Tagged>(&self) -> Option<T::Data<'_>> {
        if get_tag(self.0 as i64) == T::TAG {
            Some(unsafe { self.untagged_as_ref_unchecked::<T>() })
        } else {
            None
        }
    }
    pub fn untagged_as_mut<T: Tagged>(&self) -> Option<T::DataMut<'_>> {
        if get_tag(self.0 as i64) == T::TAG {
            Some(unsafe { self.untagged_as_mut_unchecked::<T>() })
        } else {
            None
        }
    }
    pub unsafe fn untagged_as_ref_unchecked<T: Tagged>(&self) -> T::Data<'_> {
        T::cast(self.0)
    }
    pub unsafe fn untagged_as_mut_unchecked<T: Tagged>(&self) -> T::DataMut<'_> {
        T::cast_mut(self.0)
    }
}

// #[derive(Debug, Clone)]
// struct Obj<T: Tagged>(pub T);
// #[derive(Debug, Clone, Copy)]
// struct ObjRef<'a, T: Tagged>(pub T::Data<'a>);
// #[derive(Debug, Clone)]
// struct ObjMut<'a, T: Tagged>(pub T::DataMut<'a>);
