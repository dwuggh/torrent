//! Root-chain support shared by Rust/native frames and generated JIT frames.
//!
//! Native frames own spilled `Object` words. JIT frames describe generated-code
//! stack slots that are located from Cranelift stack-map metadata.

use smallvec::SmallVec;

use mmtk::util::Address;

use crate::core::env::{DynamicBinding, DynamicEnv};
use crate::core::object::Object;
use crate::gc::scanning::StackRootVisitor;
use crate::gc::thread::MutatorThread;

const ROOT_LINK_TAG_MASK: usize = 0b1;
const ROOT_LINK_NATIVE: usize = 0b0;
const ROOT_LINK_JIT: usize = 0b1;

/// Tagged pointer to one frame in a mutator's root chain.
///
/// Frames are aligned, so the low bit encodes whether the pointer is a native
/// frame header or a JIT frame. `NULL` terminates the chain.
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RootLink(usize);

impl RootLink {
    pub const NULL: Self = Self(0);

    #[inline]
    pub fn is_null(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub fn bits(self) -> usize {
        self.0
    }

    #[inline]
    pub(crate) fn tag(self) -> usize {
        self.0 & ROOT_LINK_TAG_MASK
    }

    #[inline]
    pub(crate) fn ptr<T>(self) -> *mut T {
        (self.0 & !ROOT_LINK_TAG_MASK) as *mut T
    }

    #[inline]
    pub fn from_bits(bits: usize) -> Self {
        Self(bits)
    }

    #[inline]
    pub fn native(frame: *mut NativeRootFrameHeader) -> Self {
        debug_assert_eq!((frame as usize) & ROOT_LINK_TAG_MASK, 0);
        Self((frame as usize) | ROOT_LINK_NATIVE)
    }

    #[inline]
    pub fn jit(frame: *mut JitRootFrame) -> Self {
        debug_assert_eq!((frame as usize) & ROOT_LINK_TAG_MASK, 0);
        Self((frame as usize) | ROOT_LINK_JIT)
    }
}

/// Erased header shared by every const-generic native root frame.
#[repr(C, align(16))]
pub struct NativeRootFrameHeader {
    /// Previous frame in the mutator-local root chain.
    pub prev: RootLink,
    scan: unsafe fn(*mut NativeRootFrameHeader, &mut dyn StackRootVisitor),
}

impl NativeRootFrameHeader {
    #[inline]
    fn new(scan: unsafe fn(*mut NativeRootFrameHeader, &mut dyn StackRootVisitor)) -> Self {
        Self {
            prev: RootLink::NULL,
            scan,
        }
    }
}

/// Explicit root frame for Rust/native code.
///
/// The frame owns the spilled `Object` words that MMTk may update through
/// `TaggedPtrSlot`. `N` is the inline capacity of the `SmallVec`.
#[repr(C, align(16))]
pub struct NativeRootFrame<const N: usize> {
    pub header: NativeRootFrameHeader,
    pub slots: SmallVec<[Object; N]>,
}

impl<const N: usize> NativeRootFrame<N> {
    #[inline]
    pub fn new(slots: SmallVec<[Object; N]>) -> Self {
        Self {
            header: NativeRootFrameHeader::new(scan_native_root_frame_impl::<N>),
            slots,
        }
    }
}

/// JIT root frame describing one generated-code safepoint.
///
/// The actual stack-map metadata is owned by the JIT code registry. This frame
/// captures the dynamic bases needed to interpret one live generated frame.
#[repr(C, align(16))]
pub struct JitRootFrame {
    pub prev: RootLink,
    pub frame_pointer: *const usize,
    pub stop_frame_pointer: *const usize,
    pub stack_pointer: *const usize,
    pub return_pc: *const u8,
    pub dynamic_env: *const DynamicEnv,
}

impl JitRootFrame {
    #[inline]
    pub const fn new(
        frame_pointer: *const usize,
        stack_pointer: *const usize,
        return_pc: *const u8,
        dynamic_env: *const DynamicEnv,
    ) -> Self {
        Self {
            prev: RootLink::NULL,
            frame_pointer,
            stop_frame_pointer: std::ptr::null(),
            stack_pointer,
            return_pc,
            dynamic_env,
        }
    }
}

/// RAII handle for a pushed root frame.
///
/// Dropping the guard pops exactly the frame it pushed. Root frames form a
/// stack and must be removed in strict LIFO order.
pub struct RootFrameGuard {
    thread: *const MutatorThread,
    expected_head: RootLink,
    prev: RootLink,
}

impl Drop for RootFrameGuard {
    fn drop(&mut self) {
        unsafe {
            let thread = &*self.thread;
            let current = RootLink::from_bits(thread.root_head.get());
            debug_assert_eq!(
                current, self.expected_head,
                "root frames must be popped in LIFO order"
            );
            thread.root_head.set(self.prev.bits());
        }
    }
}

/// Push a native root frame onto the current mutator's root chain.
///
/// # Safety
///
/// `frame` must remain valid and unmoved until the returned guard is dropped.
/// The caller must push only on the current mutator thread and must drop
/// root-frame guards in strict LIFO order.
pub unsafe fn push_native_root_frame<const N: usize>(
    thread: &MutatorThread,
    frame: *mut NativeRootFrame<N>,
) -> RootFrameGuard {
    let prev = RootLink::from_bits(thread.root_head.get());
    let header = unsafe { &mut (*frame).header };
    header.prev = prev;

    let head = RootLink::native(header);
    thread.root_head.set(head.bits());

    RootFrameGuard {
        thread,
        expected_head: head,
        prev,
    }
}

/// Push a JIT root frame onto the current mutator's root chain.
///
/// # Safety
///
/// `frame` must remain valid and unmoved until the returned guard is dropped.
/// `return_pc` must identify a known JIT safepoint when a GC scans the frame,
/// and the frame bases must match the Cranelift stack-map offset convention.
pub unsafe fn push_jit_root_frame(
    thread: &MutatorThread,
    frame: *mut JitRootFrame,
) -> RootFrameGuard {
    let prev = RootLink::from_bits(thread.root_head.get());
    unsafe {
        (*frame).prev = prev;
        (*frame).stop_frame_pointer = nearest_jit_frame_pointer(prev);
    }

    let head = RootLink::jit(frame);
    thread.root_head.set(head.bits());

    RootFrameGuard {
        thread,
        expected_head: head,
        prev,
    }
}

unsafe fn nearest_jit_frame_pointer(mut link: RootLink) -> *const usize {
    while !link.is_null() {
        match link.tag() {
            ROOT_LINK_NATIVE => {
                let header = link.ptr::<NativeRootFrameHeader>();
                link = unsafe { (*header).prev };
            }
            ROOT_LINK_JIT => {
                let frame = unsafe { &*link.ptr::<JitRootFrame>() };
                return frame.frame_pointer;
            }
            other => panic!("bad root-link tag: {other:#x}"),
        }
    }

    std::ptr::null()
}

/// Walk a linked root chain and visit every published root slot.
///
/// # Safety
///
/// `link` must be the head of a well-formed chain whose frames remain live for
/// the duration of the scan. The JIT scanner must understand every JIT frame's
/// metadata convention.
pub unsafe fn scan_root_chain(
    mut link: RootLink,
    visitor: &mut dyn StackRootVisitor,
    scan_jit_frame: unsafe fn(&JitRootFrame, &mut dyn StackRootVisitor),
) {
    while !link.is_null() {
        match link.tag() {
            ROOT_LINK_NATIVE => {
                let header = link.ptr::<NativeRootFrameHeader>();
                unsafe { scan_native_root_frame(header, visitor) };
                link = unsafe { (*header).prev };
            }
            ROOT_LINK_JIT => {
                let frame = unsafe { &*link.ptr::<JitRootFrame>() };
                unsafe { scan_jit_frame(frame, visitor) };
                unsafe { scan_dynamic_env(frame.dynamic_env, visitor) };
                link = frame.prev;
            }
            other => panic!("bad root-link tag: {other:#x}"),
        }
    }
}

/// Visit root slots published by a native Rust frame.
///
/// # Safety
///
/// `header` must point to the header of a live `NativeRootFrame<N>`.
pub unsafe fn scan_native_root_frame(
    header: *mut NativeRootFrameHeader,
    visitor: &mut dyn StackRootVisitor,
) {
    let scan = unsafe { (*header).scan };
    unsafe { scan(header, visitor) };
}

unsafe fn scan_native_root_frame_impl<const N: usize>(
    header: *mut NativeRootFrameHeader,
    visitor: &mut dyn StackRootVisitor,
) {
    let frame = header.cast::<NativeRootFrame<N>>();
    for obj in unsafe { (*frame).slots.iter_mut() } {
        visitor.visit_tagged_slot(Address::from_ptr(obj as *mut Object));
    }
}

unsafe fn scan_dynamic_env(mut env: *const DynamicEnv, visitor: &mut dyn StackRootVisitor) {
    while !env.is_null() {
        let current = unsafe { &*env };
        let bindings = current.bindings.as_ptr();

        for index in 0..current.len {
            let binding = unsafe { bindings.add(index) as *mut DynamicBinding };
            let obj = unsafe { &mut (*binding).obj };
            visitor.visit_tagged_slot(Address::from_ptr(obj as *mut Object));
        }

        env = current.parent;
    }
}

#[cfg(test)]
mod tests {
    use std::ptr;

    use mmtk::vm::slot::Slot;

    use crate::gc::{GcTag, TaggedPtrSlot};

    use super::*;

    struct CollectingVisitor(Vec<Address>);

    impl StackRootVisitor for CollectingVisitor {
        fn visit_tagged_slot(&mut self, slot: Address) {
            self.0.push(slot);
        }
    }

    unsafe fn fake_jit_scan(frame: &JitRootFrame, visitor: &mut dyn StackRootVisitor) {
        visitor.visit_tagged_slot(Address::from_ptr(frame.stack_pointer as *mut Object));
    }

    unsafe fn empty_jit_scan(_frame: &JitRootFrame, _visitor: &mut dyn StackRootVisitor) {}

    #[test]
    fn native_root_frame_reports_registered_slot_addresses() {
        let mut frame = NativeRootFrame::<2>::new(smallvec::smallvec![Object(1), Object(2)]);
        let mut visitor = CollectingVisitor(Vec::new());

        unsafe {
            scan_native_root_frame(&mut frame.header, &mut visitor);
        }

        let base = frame.slots.as_mut_ptr();
        assert_eq!(
            visitor.0,
            vec![
                Address::from_ptr(base),
                Address::from_ptr(unsafe { base.add(1) }),
            ]
        );
    }

    #[test]
    fn native_root_frame_zero_inline_capacity_scans_owned_slots() {
        let mut frame =
            NativeRootFrame::<0>::new(SmallVec::from_slice(&[Object(1), Object(2), Object(3)]));
        let mut visitor = CollectingVisitor(Vec::new());

        unsafe {
            scan_native_root_frame(&mut frame.header, &mut visitor);
        }

        let base = frame.slots.as_mut_ptr();
        assert_eq!(
            visitor.0,
            vec![
                Address::from_ptr(base),
                Address::from_ptr(unsafe { base.add(1) }),
                Address::from_ptr(unsafe { base.add(2) }),
            ]
        );
    }

    #[test]
    fn root_chain_scans_jit_and_native_frames() {
        let mut native = NativeRootFrame::<1>::new(smallvec::smallvec![Object(1)]);
        let mut jit_value = Object(2);
        let mut jit = JitRootFrame::new(
            ptr::null(),
            &mut jit_value as *mut Object as *const usize,
            ptr::null(),
            ptr::null(),
        );
        jit.prev = RootLink::native(&mut native.header);
        let mut visitor = CollectingVisitor(Vec::new());

        unsafe {
            scan_root_chain(RootLink::jit(&mut jit), &mut visitor, fake_jit_scan);
        }

        let native_base = native.slots.as_mut_ptr();
        assert_eq!(
            visitor.0,
            vec![
                Address::from_ptr(&mut jit_value as *mut Object),
                Address::from_ptr(native_base),
            ]
        );
    }

    #[repr(C)]
    struct DynamicEnv1 {
        header: DynamicEnv,
        bindings: [DynamicBinding; 1],
    }

    #[test]
    fn root_chain_scans_dynamic_env_from_jit_frame() {
        let mut dynamic = DynamicEnv1 {
            header: DynamicEnv {
                len: 1,
                parent: ptr::null_mut(),
                bindings: [],
            },
            bindings: [DynamicBinding {
                index: 7,
                obj: Object(3),
            }],
        };
        let mut jit = JitRootFrame::new(ptr::null(), ptr::null(), ptr::null(), &mut dynamic.header);
        let mut visitor = CollectingVisitor(Vec::new());

        unsafe {
            scan_root_chain(RootLink::jit(&mut jit), &mut visitor, empty_jit_scan);
        }

        assert_eq!(
            visitor.0,
            vec![Address::from_ptr(
                &mut dynamic.bindings[0].obj as *mut Object
            )]
        );
    }

    #[test]
    fn tagged_ptr_slot_updates_native_frame_object_word() {
        let mut frame =
            NativeRootFrame::<1>::new(smallvec::smallvec![Object(0x1000 | GcTag::CONS as u64,)]);
        let slot = TaggedPtrSlot::from_address(Address::from_ptr(frame.slots.as_mut_ptr()));
        let moved = unsafe {
            mmtk::util::ObjectReference::from_raw_address_unchecked(Address::from_usize(0x2000))
        };

        slot.store(moved);

        assert_eq!(frame.slots[0].raw(), 0x2000 | GcTag::CONS as u64);
    }
}
