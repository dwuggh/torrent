use std::{
    alloc::Layout, cell::UnsafeCell, fmt::Debug, hash::Hash, marker::PhantomData, ptr::NonNull,
    sync::LazyLock,
};

use mmtk::{
    util::{Address, ObjectReference},
    vm::slot::Slot,
};

use crate::{Tagged, TaggedPtrSlot, alloc_with_tag, thread::current_thread};

pub trait TagSpec: Copy + Send + Sync + Debug + PartialEq + Eq + Hash {
    const MASK: u8;
    const HEADER_MARKER: u8;

    fn nonref() -> &'static [u8];

    fn is_header_word(word: u64) -> bool {
        (word as u8 & Self::MASK) == Self::HEADER_MARKER
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GcTag;

impl TagSpec for GcTag {
    const MASK: u8 = 0b111;
    const HEADER_MARKER: u8 = 0b101;

    fn nonref() -> &'static [u8] {
        &[0b000, 0b001, 0b011, 0b101, 0b111]
    }
}

pub struct Gc<T: ?Sized> {
    ptr: NonNull<GcInner<T>>,
    phantom: PhantomData<GcInner<T>>,
}

#[derive(Debug)]
pub struct GcInner<T: ?Sized> {
    header: UnsafeCell<GcHeader>,
    data: UnsafeCell<T>,
}

#[derive(Debug)]
pub struct Headerless<T: ?Sized> {
    data: UnsafeCell<T>,
}

impl<T: ?Sized> Gc<T> {
    fn new(ptr: NonNull<GcInner<T>>) -> Self {
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    fn into_addr(self) -> Address {
        let addr = self.ptr.addr().get() + std::mem::size_of::<usize>();
        std::mem::forget(self);
        unsafe { Address::from_usize(addr) }
    }

    fn as_addr(&self) -> Address {
        unsafe { Address::from_usize(self.ptr.addr().get() + std::mem::size_of::<usize>()) }
    }

    fn update(&mut self, src: ObjectReference, target: Self) {
        let thread = current_thread();
        let mut m = thread.guard_mutator().unwrap();
        let slot = TaggedPtrSlot::from_address(self.as_addr());
        let target = ObjectReference::from_raw_address(target.into_addr());
        // m.barrier.object_reference_write(src, slot, target.unwrap());
        mmtk::memory_manager::object_reference_write_pre(&mut m, src, slot, target);
        slot.store(target.unwrap());
        mmtk::memory_manager::object_reference_write_post(&mut m, src, slot, target);
    }
}

pub trait Trace {
    fn trace(&self, visitor: &mut Visitor);
    fn finalize(&mut self) {}
}

pub trait VisitorImpl {
    fn visit_slot_address(&mut self, address: Address);
}

pub struct Visitor<'a>(&'a mut dyn VisitorImpl);

impl<'a> Visitor<'a> {
    pub fn new(visitor: &'a mut dyn VisitorImpl) -> Self {
        Self(visitor)
    }

    pub fn visit_slot_address(&mut self, address: Address) {
        self.0.visit_slot_address(address);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GcObjectSize {
    Fixed(usize),
    Variable(fn(usize) -> usize),
}

#[derive(Debug, Clone, Copy)]
pub struct MetaData {
    pub trace: fn(this: Address, visitor: Visitor),
    pub finalize: fn(this: Address),
    // pub size: GcObjectSize,
    pub size: usize,
}

pub static VTABLE: LazyLock<Vec<MetaData>> = LazyLock::new(|| Vec::new());

pub fn get_metadata_for_tag<'a>(tag: u8) -> Option<&'a MetaData> {
    VTABLE.get(tag as usize)
}

// pub type Visitor<'a> = &'a mut dyn SlotVisitor<TaggedPtrSlot>;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct GcHeader {
    data: u64,
}

impl GcHeader {
    const FORWARDING_BITS_MASK: u64 = 0b11u64 << 62;

    pub fn is_header_word(word: u64) -> bool {
        GcTag::is_header_word(word)
    }

    pub fn tag(self) -> u8 {
        debug_assert!(Self::is_header_word(self.data));
        let data = self.data & !Self::FORWARDING_BITS_MASK;
        let tag = data >> 3;
        tag as u8
    }

    pub fn metadata(&self) -> &MetaData {
        get_metadata_for_tag(self.tag()).expect("no metadata for tag")
    }
}

const fn gc_inner_layout<T>() -> Layout {
    Layout::new::<GcInner<T>>()
}

#[repr(transparent)]
#[derive(PartialEq, PartialOrd, Eq, Hash)]
struct Object(u64);

impl Object {
    fn untag<T: Tagged>(self) -> Gc<T> {
        todo!()
    }

    fn tag<T: Tagged>(obj: Gc<T>) -> Self {
        todo!()
    }

    fn get_tag(&self) -> u8 {
        self.0 as u8 & GcTag::MASK
    }

    fn get_addr(&self) -> Address {
        let tag = self.get_tag() as usize;
        let addr = self.0 as usize - tag;
        unsafe { Address::from_usize(addr) }
    }

    fn get_objref(&self) -> ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(self.get_addr()) }
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        let thread = current_thread();
        let mut m = thread.guard_mutator().unwrap();
        let src = self.get_objref();
        let target = alloc_with_tag(&mut m, self.get_tag());
        let slot = TaggedPtrSlot::from_address(target);
        let target = ObjectReference::from_raw_address(target);
        // m.barrier.object_reference_write(src, slot, target.unwrap());
        mmtk::memory_manager::object_reference_write_pre(&mut m, src, slot, target);
        slot.store(target.unwrap());
        mmtk::memory_manager::object_reference_write_post(&mut m, src, slot, target);
        Self(self.0)
    }
}
