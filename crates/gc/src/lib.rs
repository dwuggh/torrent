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

const OBJECT_REF_OFFSET: usize = std::mem::size_of::<usize>();

pub fn bind_mutator(mmtk: &'static mmtk::MMTK<MM>) {
    let manager = &THREAD_MANAGER;
    let thread = manager.add_or_get_current_thread();
    let mutator = mmtk::memory_manager::bind_mutator(mmtk, thread.to_mutator_thread());
    thread.bind_mutator(mutator);
}

pub trait Tagged {
    const TAG: u8;
}

#[inline(always)]
pub fn alloc_with_size<VM: VMBinding>(mutator: &mut Mutator<VM>, size: usize) -> Address {
    mmtk::memory_manager::alloc(mutator, size, 8, 0, mmtk::AllocationSemantics::Default)
}

pub fn alloc_with_tag<VM: VMBinding>(mutator: &mut Mutator<VM>, tag: u8) -> Address {
    let meta = get_metadata_for_tag(tag).unwrap();
    let size = meta.size;
    mmtk::memory_manager::alloc(mutator, size, 8, 0, mmtk::AllocationSemantics::Default)
        + OBJECT_REF_OFFSET
}

pub fn alloc<O: Tagged>() -> Option<u64> {
    let tag = O::TAG;
    let meta = get_metadata_for_tag(tag)?;
    let size = meta.size;
    let thread = current_thread();
    let addr = (thread.alloc(size) + OBJECT_REF_OFFSET).as_usize() as u64;
    let result = addr | tag as u64;
    Some(result)
}

pub fn destroy_mutator() {
    let manager = &THREAD_MANAGER;
    let thread = manager.add_or_get_current_thread();
    let mut g = thread.mutator.lock().unwrap();
    let mut mutator = g.take().unwrap();
    mmtk::memory_manager::destroy_mutator(&mut mutator);
}

pub use gc::*;
pub use mmtk;
pub use slot::TaggedPtrSlot;
