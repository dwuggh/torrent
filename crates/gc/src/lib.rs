use mmtk::{Mutator, util::Address, vm::VMBinding};

use crate::{
    thread::{THREAD_MANAGER, current_thread},
    vm::MM,
};

mod active_plan;
mod collection;
mod gc;
mod object_model;
mod reference_glue;
mod scanning;
mod slot;
mod thread;
mod vm;

/// Offset from allocation start to the MMTk `ObjectReference` address.
///
/// The raw tagged Lisp object stores this address with low tag bits set.
pub(crate) const OBJECT_REF_OFFSET: usize = std::mem::size_of::<usize>();

/// Bind an MMTk mutator to the current runtime thread.
pub fn bind_mutator(mmtk: &'static mmtk::MMTK<MM>) {
    let manager = &THREAD_MANAGER;
    let thread = manager.add_or_get_current_thread();
    let mutator = mmtk::memory_manager::bind_mutator(mmtk, thread.to_mutator_thread());
    thread.bind_mutator(mutator);
}

/// Logical tag carried by a Torrent heap or immediate value type.
pub trait Tagged {
    /// Three-bit logical tag used by GC metadata and object headers.
    const TAG: u8;
}

/// Allocate `size` bytes through MMTk with the default allocation semantics.
#[inline(always)]
pub fn alloc_with_size<VM: VMBinding>(mutator: &mut Mutator<VM>, size: usize) -> Address {
    mmtk::memory_manager::alloc(mutator, size, 8, 0, mmtk::AllocationSemantics::Default)
}

/// Allocate an object by logical metadata tag and return its object-reference address.
pub fn alloc_with_tag<VM: VMBinding>(mutator: &mut Mutator<VM>, tag: u8) -> Address {
    let meta = get_metadata_for_tag(tag).unwrap();
    let size = meta.layout.size();
    mmtk::memory_manager::alloc(mutator, size, 8, 0, mmtk::AllocationSemantics::Default)
        + OBJECT_REF_OFFSET
}

/// Allocate an object for `O` and return a raw tagged object word.
///
/// This low-level helper only reserves memory. Typed initialization should use
/// `Gc<T>::new` so the object representation is written before publication.
pub fn alloc<O: Tagged>() -> Option<u64> {
    let tag = O::TAG;
    let meta = get_metadata_for_tag(tag)?;
    let size = meta.layout.size();
    let thread = current_thread();
    let addr = (thread.alloc(size) + OBJECT_REF_OFFSET).as_usize() as u64;
    let result = addr | tag as u64;
    Some(result)
}

/// Destroy the MMTk mutator bound to the current thread.
pub fn destroy_mutator() {
    let thread = THREAD_MANAGER.add_or_get_current_thread();
    let mut mutator = thread.take_mutator().unwrap();
    mmtk::memory_manager::destroy_mutator(&mut mutator);
}

pub use gc::*;
pub use mmtk;
pub use slot::TaggedPtrSlot;
