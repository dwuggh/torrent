use std::ops::{Deref, DerefMut};

use crate::{
    core::{object::Object, tag::TAG_VECTOR},
    gc::{Gc, HeaderedObject, Trace, Visitor},
};

#[derive(Clone, Debug)]
pub struct LispVector(pub Gc<Vector>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Vector(pub Vec<Object>);

impl LispVector {
    pub fn new(objs: Vec<Object>, ctx: &mut crate::gc::GcContext) -> Self {
        let roots = ctx.root_slice(&objs);
        Self(ctx.alloc::<Vector>(&roots))
    }
}

impl Deref for Vector {
    type Target = Vec<Object>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Vector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl crate::gc::Tagged for Vector {
    const TAG: u8 = TAG_VECTOR;
}

unsafe impl HeaderedObject for Vector {}

unsafe impl Trace for Vector {
    unsafe fn trace(&self, visitor: &mut Visitor) {
        // Every element is a tagged object slot that MMTk may update if the
        // referenced object moves.
        for object in &self.0 {
            unsafe { object.trace(visitor) };
        }
    }
}

impl_tagged_for_gc!(LispVector, Vector);

impl crate::gc::ConstructObject for Vector {
    type Init<'a> = &'a crate::gc::RootedSlice;
    fn size_from_init(_init: &Self::Init<'_>) -> usize {
        std::mem::size_of::<crate::gc::Headered<Self>>()
    }
    fn size(&self) -> usize {
        std::mem::size_of::<crate::gc::Headered<Self>>()
    }
    unsafe fn init_fn(init: Self::Init<'_>, ctx: &crate::gc::GcContext) -> Self {
        Self(init.iter().map(|root| *root.get(ctx)).collect())
    }
}
