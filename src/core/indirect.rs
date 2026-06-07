use std::ptr::NonNull;

use crate::{
    core::{
        object::{Object, write_object_slot},
        tag::{TAG_INDIRECT, Tag},
    },
    gc::{Gc, HeaderedObject, HeapObject, Trace, Visitor},
};

#[derive(Debug, Clone)]
pub struct LispIndirect(pub(crate) Gc<Indirect>);
impl_tagged_for_gc!(LispIndirect, Indirect);

#[repr(C)]
#[derive(Debug)]
pub struct Indirect {
    value: Object,
}

impl Indirect {
    pub fn new(value: Object) -> Self {
        Self { value }
    }

    pub fn new_object(value: Object) -> Object {
        LispIndirect(Gc::new(Self::new(value))).tag()
    }

    pub fn is_object(object: Object) -> bool {
        object.get_tag() == TAG_INDIRECT
    }

    /// # Safety
    ///
    /// `object` must be a live tagged object whose logical tag is `TAG_INDIRECT`.
    pub unsafe fn from_object<'a>(object: Object) -> &'a Self {
        debug_assert!(Self::is_object(object.clone()));
        let object_ref = unsafe { object.object_ref_unchecked() };
        unsafe { Self::data_from_object_ref(object_ref).as_ref() }
    }

    /// # Safety
    ///
    /// `object` must be a live tagged object whose logical tag is `TAG_INDIRECT`.
    pub unsafe fn from_object_mut<'a>(object: Object) -> &'a mut Self {
        debug_assert!(Self::is_object(object.clone()));
        let object_ref = unsafe { object.object_ref_unchecked() };
        unsafe { Self::data_from_object_ref(object_ref).as_mut() }
    }

    pub fn load(&self) -> Object {
        self.value.clone()
    }

    pub fn store(&mut self, value: Object) {
        let src = unsafe { Self::object_ref_from_data(NonNull::from(&mut *self)) };
        write_object_slot(src, &mut self.value, value);
    }
}

impl crate::gc::Tagged for Indirect {
    const TAG: u8 = TAG_INDIRECT;
}

unsafe impl HeaderedObject for Indirect {}

unsafe impl Trace for Indirect {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        unsafe { self.value.trace(visitor) };
    }
}
