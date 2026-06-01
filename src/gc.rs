use std::sync::OnceLock;

use mmtk::{
    util::{alloc::AllocatorSelector, Address, ObjectReference},
    vm::slot::Slot,
    vm::VMBinding,
    AllocationSemantics, Mutator,
};

use self::thread::{current_thread, AllocFastPath, THREAD_MANAGER};

mod active_plan;
mod collection;
mod lab;
mod object_model;
mod object;
mod reference_glue;
mod scanning;
mod slot;
mod thread;
mod vm;

#[cfg(not(target_pointer_width = "64"))]
compile_error!("torrent GC currently assumes 64-bit object references");

/// Offset from allocation start to the MMTk `ObjectReference` address.
///
/// The raw tagged Lisp object stores this address with low tag bits set.
pub const OBJECT_REF_OFFSET: usize = 8;

pub(crate) static MMTK: OnceLock<&'static mmtk::MMTK<MM>> = OnceLock::new();

pub(crate) fn mmtk_instance() -> &'static mmtk::MMTK<MM> {
    MMTK.get().copied().expect("MMTk is not initialized")
}

pub(crate) fn default_allocator_selector() -> AllocatorSelector {
    mmtk::memory_manager::get_allocator_mapping(mmtk_instance(), AllocationSemantics::Default)
}

/// Bind an MMTk mutator to the current runtime thread.
pub fn bind_mutator(mmtk: &'static mmtk::MMTK<MM>) {
    let _ = MMTK.set(mmtk);
    let manager = &THREAD_MANAGER;
    let thread = manager.add_or_get_current_thread();
    let mutator = mmtk::memory_manager::bind_mutator(mmtk, thread.to_mutator_thread());
    let alloc_fastpath = match default_allocator_selector() {
        AllocatorSelector::BumpPointer(_) | AllocatorSelector::Immix(_) => AllocFastPath::Tlab,
        _ => AllocFastPath::None,
    };
    thread.bind_mutator(mutator, alloc_fastpath);
}

/// Initialize MMTk collection state from the current runtime thread.
///
/// The runtime crate owns the process-wide [`mmtk::MMTK`] instance. Call this
/// once after `mmtk_init` and before binding mutators or allocating through
/// [`Gc`]. The helper keeps the thread-manager internals private to this crate.
pub fn initialize_collection(mmtk: &'static mmtk::MMTK<MM>) {
    let thread = THREAD_MANAGER.add_or_get_current_thread();
    mmtk.initialize_collection(thread.to_vmthread());
}

/// Return the MMTk mutator-thread token for the current runtime thread.
#[inline]
pub fn current_mutator_tls() -> mmtk::util::VMMutatorThread {
    current_thread().to_mutator_thread()
}

/// Logical metadata tag carried by a Torrent heap type.
///
/// This is not the low three-bit primary tag stored in raw Lisp `Object`
/// words. Headered objects store this full logical tag in [`GcHeader`], and
/// MMTk metadata lookup uses it to find trace/finalize/size callbacks.
pub trait Tagged {
    /// Runtime-assigned-by-hand logical tag used by GC metadata and headers.
    const TAG: u8;
}

/// Allocate `size` bytes through MMTk with the default allocation semantics.
///
/// This returns the allocation start. Callers that publish an object reference
/// must initialize the object, compute `start + OBJECT_REF_OFFSET`, and then run
/// `post_alloc` before a safepoint can observe the object.
#[inline(always)]
pub fn alloc_with_size<VM: VMBinding>(mutator: &mut Mutator<VM>, size: usize) -> Address {
    alloc_start_with_semantics(mutator, size, AllocationSemantics::Default)
}

/// Allocate `size` bytes and return the allocation start.
#[inline(always)]
pub(crate) fn alloc_start_with_semantics<VM: VMBinding>(
    mutator: &mut Mutator<VM>,
    size: usize,
    semantics: AllocationSemantics,
) -> Address {
    mmtk::memory_manager::alloc(mutator, size, 8, OBJECT_REF_OFFSET, semantics)
}

/// Run MMTk's metadata initialization for a freshly allocated object.
///
/// This is independent from write barriers. It initializes per-space metadata
/// such as LOS mark/nursery bits for the selected allocation semantics.
#[inline(always)]
pub(crate) fn post_alloc_with_semantics<VM: VMBinding>(
    mutator: &mut Mutator<VM>,
    object: ObjectReference,
    size: usize,
    semantics: AllocationSemantics,
) {
    mmtk::memory_manager::post_alloc(mutator, object, size, semantics);
}

/// Run MMTk's pre-write barrier for a tagged object slot.
///
/// The VM side performs the actual store because a slot can contain immediate
/// values as well as object references. `target` is the old object reference
/// loaded from the slot before overwriting it, or `None` if the old word was an
/// immediate/non-reference value.
#[inline(always)]
pub fn object_reference_write_pre(
    src: ObjectReference,
    slot: TaggedPtrSlot,
    target: Option<ObjectReference>,
) {
    let thread = current_thread();
    mmtk::memory_manager::object_reference_write_pre(thread.mutator(), src, slot, target);
}

/// Run the pre-write barrier for a raw slot address.
///
/// This helper is used by runtime heap setters: it wraps the address as a
/// [`TaggedPtrSlot`], reads the old reference target, and calls the MMTk
/// pre-barrier before the caller writes the new tagged word.
#[inline(always)]
pub fn object_slot_write_pre(src: ObjectReference, slot_addr: Address) {
    let slot = TaggedPtrSlot::from_address(slot_addr);
    let old_target = slot.load();
    object_reference_write_pre(src, slot, old_target);
}

/// Allocate an object by logical metadata tag and return its object-reference address.
pub fn alloc_with_tag<VM: VMBinding>(mutator: &mut Mutator<VM>, tag: u8) -> Address {
    let meta = get_metadata_for_tag(tag).unwrap();
    let size = meta.layout.size();
    let start = alloc_start_with_semantics(mutator, size, AllocationSemantics::Default);
    let object = unsafe { ObjectReference::from_raw_address_unchecked(start + OBJECT_REF_OFFSET) };
    post_alloc_with_semantics(mutator, object, size, AllocationSemantics::Default);
    object.to_raw_address()
}

/// Allocate an object for `O` and return a raw tagged object word.
///
/// This low-level helper only reserves memory. Typed initialization should use
/// `Gc<T>::new` so the object representation is written before publication.
pub fn alloc<O: HeapObject>() -> Option<u64> {
    let tag = O::TAG;
    let meta = get_metadata_for_tag(tag)?;
    let size = meta.layout.size();
    let thread = current_thread();
    let start = thread.alloc(size);
    let object = unsafe {
        mmtk::util::ObjectReference::from_raw_address_unchecked(start + OBJECT_REF_OFFSET)
    };
    post_alloc_with_semantics(thread.mutator(), object, size, AllocationSemantics::Default);
    let addr = object.to_raw_address().as_usize() as u64;
    let result = addr | O::PRIMARY_TAG as u64;
    Some(result)
}

/// Destroy the MMTk mutator bound to the current thread.
pub fn destroy_mutator() {
    let thread = THREAD_MANAGER.add_or_get_current_thread();
    let mut mutator = thread.take_mutator().unwrap();
    mmtk::memory_manager::destroy_mutator(&mut mutator);
}

pub use mmtk;
pub use object::*;
pub use scanning::{set_stackmap_provider, StackMapProvider, StackRootVisitor};
pub use slot::TaggedPtrSlot;
pub use vm::MM;
