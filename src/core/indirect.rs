use crate::{
    core::{
        object::Object,
        tag::{TAG_INDIRECT, Tag},
    },
    gc::{Gc, HeaderedObject, HeapObject, Trace, Visitor},
};
use proc_macros::HeapEntries;

#[derive(Debug, Clone)]
pub struct LispIndirect(pub(crate) Gc<Indirect>);
impl_tagged_for_gc!(LispIndirect, Indirect);

#[repr(C)]
#[derive(Debug, HeapEntries)]
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
        self.value_mut().set(value);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{number::LispInteger, tag::Tag};

    #[test]
    fn generated_entry_updates_value() {
        let _mutator = crate::gc::test_support::bind_nogc_mutator();
        let mut indirect = LispIndirect(Gc::new(Indirect::new(LispInteger(1).tag())));
        let value = LispInteger(2).tag();

        indirect.0.as_mut().value_mut().set(value);

        assert_eq!(indirect.0.as_ref().load(), value);
    }
}
