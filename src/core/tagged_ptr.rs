use std::mem::ManuallyDrop;
use thiserror::Error;

use crate::{
    core::object::{LispType, Object},
    gc::{Gc, GcInner},
};

pub fn get_tag(val: i64) -> LispType {
    unsafe { std::mem::transmute(val as u8) }
}

#[derive(Error, Debug, Clone)]
pub enum TaggedPtrError {
    #[error("encounted null pointer")]
    NullPointer,
    #[error("type mismatch")]
    TypeMisMatch,
}

pub trait TaggedPtr: Sized + TryFrom<*mut Self::Inner> {
    const TAG: LispType;

    type Data;
    type Inner: AsMut<Self::Data> + AsRef<Self::Data>;

    /// get the actual pointer for `Self`, untagged
    unsafe fn to_raw(&self) -> u64 {
        unimplemented!()
    }

    /// consume `Value` and generate `Self`.
    /// NOTE To reduce RC overhead, we do not want `Drop::drop` to be called here,
    /// as well as `Gc::inc_rc`
    fn untag(val: Object) -> Result<Self, TaggedPtrError> {
        if get_tag(val.0 as i64) == Self::TAG {
            let val = ManuallyDrop::new(val);
            let ptr = Self::untag_ptr(val.0);
            ptr.try_into().map_err(|_| TaggedPtrError::NullPointer)
        } else {
            Err(TaggedPtrError::TypeMisMatch)
        }
    }

    /// get the raw data for tagged pointer
    fn raw(&self) -> u64 {
        unsafe { self.to_raw() << 8 | Self::TAG as u64 }
    }

    /// Given the type, consume `Self`, return `Value`.
    /// NOTE this must ensure reference count is aligned
    fn tag(self) -> Object {
        let this = ManuallyDrop::new(self);
        Object(this.raw())
    }

    fn untag_ptr(val: u64) -> *mut Self::Inner {
        let data = val >> 8;
        data as *mut Self::Inner
    }

    fn inner_ref<'a>(val: u64) -> &'a Self::Inner {
        unsafe {
            let ptr = Self::untag_ptr(val);
            ptr.as_ref().unwrap()
        }
    }

    fn inner_ref_mut<'a>(val: u64) -> &'a mut Self::Inner {
        unsafe {
            let ptr = Self::untag_ptr(val);
            ptr.as_mut().unwrap()
        }
    }

    fn as_ref(val: &Object) -> Option<&Self::Data> {
        if get_tag(val.0 as i64) == Self::TAG {
            unsafe { Some(Self::as_ref_unchecked(val)) }
        } else {
            None
        }
    }

    fn as_mut(val: &Object) -> Option<&mut Self::Data> {
        if get_tag(val.0 as i64) == Self::TAG {
            unsafe { Some(Self::as_mut_unchecked(val)) }
        } else {
            None
        }
    }

    // NOTE this function does not check for tag match
    unsafe fn as_ref_unchecked(val: &Object) -> &Self::Data {
        let data_ref = Self::untag_ptr(val.0).as_ref().map(AsRef::as_ref).unwrap();
        std::mem::transmute(data_ref)
    }

    // NOTE this function does not check for tag match
    unsafe fn as_mut_unchecked(val: &Object) -> &mut Self::Data {
        let data_ref = Self::untag_ptr(val.0).as_mut().map(AsMut::as_mut).unwrap();
        std::mem::transmute(data_ref)
    }
}

pub(crate) const MAX_FIXNUM: i64 = i64::MAX >> 8;
pub(crate) const MIN_FIXNUM: i64 = i64::MIN >> 8;

// pub struct LispVal<const Ty: LispType, T>(Gc<T>);

macro_rules! impl_tagged_ptr_for_gc {
    ($name:ident, $lispty:expr, $inner:ty) => {
        impl From<crate::gc::Gc<$inner>> for $name {
            fn from(value: crate::gc::Gc<$inner>) -> Self {
                Self(value)
            }
        }
        impl TryFrom<*mut crate::gc::GcInner<$inner>> for $name {
            type Error = &'static str;

            fn try_from(value: *mut crate::gc::GcInner<$inner>) -> Result<Self, Self::Error> {
                let val: crate::gc::Gc<$inner> = value.try_into()?;
                Ok(val.into())
            }
        }

        impl TaggedPtr for $name {
            const TAG: LispType = $lispty;
            type Data = $inner;
            type Inner = crate::gc::GcInner<$inner>;

            unsafe fn to_raw(&self) -> u64 {
                self.0.to_raw() as u64
            }
        }
    };
}

pub trait Tagged: Sized {
    const TAG: LispType;
    type Data<'a>: TryFrom<Self>;

    unsafe fn to_raw(self) -> u64 {
        unimplemented!()
    }
    
    unsafe fn from_raw(raw: u64) -> Self {
        unimplemented!()
    }


    fn untag(val: Object) -> Result<Self, TaggedPtrError>;

    /// Given the type, consume `Self`, return `Value`.
    /// NOTE this must ensure reference count is aligned
    fn tag(self) -> Object;

}
