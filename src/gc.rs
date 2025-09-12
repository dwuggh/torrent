#![allow(dead_code)]

use std::{
    alloc::Layout, any::Any, cell::UnsafeCell, fmt::Debug, hash::Hash, marker::PhantomData,
    mem::ManuallyDrop, ptr::NonNull,
};

pub mod collector;
pub mod trace;

use collector::{dec_rc, inc_rc};
pub use trace::{Trace, Visitor};

pub struct Gc<T: ?Sized> {
    ptr: NonNull<GcInner<T>>,
    phantom: PhantomData<GcInner<T>>,
}

impl<T: Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = self.get();
        Debug::fmt(val, f)
    }
}

// NOTE ops to GcHeader is not thread safe
unsafe impl<T: ?Sized + Send> Send for Gc<T> {}
unsafe impl<T: ?Sized + Sync> Sync for Gc<T> {}

#[derive(Debug)]
pub struct GcInner<T: ?Sized> {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<T>,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct GcHeader {
    rc: usize,
    crc: usize,
    color: Color,
    buffered: bool,

    log_ptr: Option<OpaqueGcPtr>,

    layout: Layout,
    visit_children: unsafe fn(this: *const (), visitor: Visitor),
    finalize: unsafe fn(this: *mut ()),
}

unsafe impl Send for GcHeader {}
unsafe impl Sync for GcHeader {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Color {
    /// In use or free
    Black,
    /// Possible member of a cycle
    Gray,
    /// Member of a garbage cycle
    White,
    /// Possible root of cycle
    Purple,
    /// Candidate cycle undergoing Σ-computation
    Red,
    /// Candidate cycle awaiting epoch boundary
    Orange,
}

impl GcHeader {
    fn new<T: Trace>() -> Self {
        Self {
            // concurrent requires rc set to 1, with an immediate dec
            rc: 1,
            crc: 1,
            color: Color::Black,
            buffered: false,
            layout: Layout::new::<GcInner<T>>(),
            log_ptr: None,
            visit_children: |this, visitor| unsafe {
                let this = this as *const T;
                T::trace(this.as_ref().unwrap(), visitor);
            },
            finalize: |this| unsafe {
                let this = this as *mut T;
                T::finalize(this.as_mut().unwrap());
            },
        }
    }
}

impl<T: Trace> GcInner<T> {
    fn new(data: T) -> Self {
        Self {
            header: UnsafeCell::new(GcHeader::new::<T>()),
            data: UnsafeCell::new(data),
        }
    }
}

impl<T: Trace> Gc<T> {
    pub fn new(data: T) -> Gc<T> {
        Self {
            ptr: NonNull::from(Box::leak(Box::new(GcInner::new(data)))),
            phantom: PhantomData,
        }
    }

    pub fn inc_ref_count(&self) {
        inc_rc(self.ptr);
    }

    pub fn dec_ref_count(&self) {
        dec_rc(self.ptr);
    }

    // pub fn into_any(this: Self) -> Gc<dyn Any> {
    //     let this = ManuallyDrop::new(this);
    //     let any: NonNull<GcInner<dyn Any>> = this.ptr;
    //     Gc {
    //         ptr: any,
    //         phantom: PhantomData,
    //     }
    // }
}

impl Gc<dyn Any> {
    pub fn downcast<T: Any>(self) -> Result<Gc<T>, Self> {
        if self.get().is::<T>() {
            let this = ManuallyDrop::new(self);
            let ptr = this.ptr.as_ptr() as *mut GcInner<T>;
            let ptr = unsafe { NonNull::new_unchecked(ptr) };
            Ok(Gc {
                ptr,
                phantom: PhantomData,
            })
        } else {
            Err(self)
        }
    }
}

impl<T: Trace> From<T> for Gc<T> {
    fn from(t: T) -> Self {
        Gc::new(t)
    }
}

impl<T> TryFrom<*mut GcInner<T>> for Gc<T> {
    type Error = &'static str;

    fn try_from(value: *mut GcInner<T>) -> Result<Self, Self::Error> {
        let ptr = NonNull::new(value).ok_or("null pointer")?;
        Ok(Self {
            ptr,
            phantom: PhantomData,
        })
    }
}

impl<T> AsRef<T> for GcInner<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.data.get().as_ref().unwrap() }
    }
}

impl<T> AsMut<T> for GcInner<T> {
    fn as_mut(&mut self) -> &mut T {
        self.data.get_mut()
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Gc<T> {
        inc_rc(self.ptr);
        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T: ?Sized> Drop for Gc<T> {
    fn drop(&mut self) {
        dec_rc(self.ptr);
    }
}

impl<T: ?Sized + Hash> Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state);
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        let this = self.get();
        let other = other.get();
        this == other
    }
}

impl<T: ?Sized + Eq> Eq for Gc<T> {}

impl<T: ?Sized> Gc<T> {
    /// # Safety
    ///
    /// This function is not safe and basically useless for anything outside of
    /// the Trace proc macro's generated code.
    pub unsafe fn as_opaque(&self) -> OpaqueGcPtr {
        OpaqueGcPtr::from(self.ptr)
    }

    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        std::ptr::addr_eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr())
    }

    pub(crate) fn as_ptr(this: &Self) -> *mut GcInner<T> {
        this.ptr.as_ptr()
    }

    pub(crate) fn to_raw(&self) -> *mut GcInner<T> {
        self.ptr.as_ptr()
    }

    pub(crate) fn into_raw(gc: Self) -> *mut GcInner<T> {
        ManuallyDrop::new(gc).ptr.as_ptr()
    }

    pub(crate) unsafe fn from_raw(ptr: *mut GcInner<T>) -> Self {
        let ptr = NonNull::new(ptr).unwrap();
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub(crate) unsafe fn from_raw_inc_rc(ptr: *mut GcInner<T>) -> Self {
        let ptr = NonNull::new(ptr).unwrap();
        inc_rc(ptr);
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    /// NOTE we don't want abuse of lock here
    pub fn get(&self) -> &T {
        unsafe { self.ptr.as_ref().data.get().as_ref().unwrap() }
    }

    pub fn get_mut(&self) -> &mut T {
        unsafe { self.ptr.as_ref().data.get().as_mut().unwrap() }
    }
}

/// Fat pointer to the header and data of the Gc
/// we customize a fat pointer to avoid types like `dyn FnMut(GcInner<dyn Trace>)`
/// which is "fatter"
#[derive(Clone, Copy, Debug)]
pub struct OpaqueGcPtr {
    header: NonNull<UnsafeCell<GcHeader>>,
    data: NonNull<UnsafeCell<()>>,
}

impl<T: ?Sized> From<NonNull<GcInner<T>>> for OpaqueGcPtr {
    fn from(mut value: NonNull<GcInner<T>>) -> Self {
        unsafe {
            let value_mut = value.as_mut();
            let header = NonNull::new(value_mut.header.get() as *mut _).unwrap();
            let data = NonNull::new(UnsafeCell::from_mut(
                &mut *(value_mut.data.get() as *mut ()),
            ))
            .unwrap();
            Self { header, data }
        }
    }
}

impl OpaqueGcPtr {
    unsafe fn rc(&self) -> usize {
        unsafe { (*self.header.as_ref().get()).rc }
    }

    unsafe fn set_rc(&self, rc: usize) {
        unsafe {
            (*self.header.as_ref().get()).rc = rc;
        }
    }

    unsafe fn crc(&self) -> usize {
        unsafe { (*self.header.as_ref().get()).crc }
    }

    unsafe fn set_crc(&self, crc: usize) {
        unsafe {
            (*self.header.as_ref().get()).crc = crc;
        }
    }

    unsafe fn color(&self) -> Color {
        unsafe { (*self.header.as_ref().get()).color }
    }

    unsafe fn set_color(&self, color: Color) {
        unsafe {
            (*self.header.as_ref().get()).color = color;
        }
    }

    unsafe fn buffered(&self) -> bool {
        unsafe { (*self.header.as_ref().get()).buffered }
    }

    unsafe fn set_buffered(&self, buffered: bool) {
        unsafe {
            (*self.header.as_ref().get()).buffered = buffered;
        }
    }

    // unsafe fn lock(&self) -> &RwLock<()> {
    //     unsafe { &(*self.header.as_ref().get()).lock }
    // }

    unsafe fn visit_children(&self) -> unsafe fn(this: *const (), visitor: Visitor) {
        unsafe { (*self.header.as_ref().get()).visit_children }
    }

    unsafe fn finalize(&self) -> unsafe fn(this: *mut ()) {
        unsafe { (*self.header.as_ref().get()).finalize }
    }

    unsafe fn layout(&self) -> Layout {
        unsafe { (*self.header.as_ref().get()).layout }
    }

    unsafe fn data(&self) -> *const () {
        unsafe { self.data.as_ref().get() as *const () }
    }

    unsafe fn data_mut(&self) -> *mut () {
        unsafe { self.data.as_ref().get() }
    }
}
