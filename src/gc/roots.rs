//! Root support shared by Rust/native slots and generated JIT frames.
//!
//! Native code stores rooted `Object` words in a per-mutator stack. Generated
//! code publishes JIT frame descriptors, and Cranelift stack-map metadata
//! locates the actual generated-code slots from those descriptors.

use std::cell::UnsafeCell;
use std::fmt;
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;
use std::rc::Rc;

use mmtk::util::Address;

use crate::core::env::{DynamicBinding, DynamicEnv};
use crate::core::object::Object;
use crate::gc::scanning::StackRootVisitor;
use crate::gc::thread::MutatorThread;
use crate::gc::{Gc, HeapObject};

/// Native roots owned by one mutator.
///
/// Every element in `slots` is currently live as a root. Reserved vector
/// capacity is not part of the active root set and is not scanned.
pub(crate) struct NativeRootStack {
    slots: Vec<UnsafeCell<Object>>,
}

impl NativeRootStack {
    pub(crate) const fn new() -> Self {
        Self { slots: Vec::new() }
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.slots.len()
    }

    #[inline]
    fn push(&mut self, value: Object) -> usize {
        let index = self.slots.len();
        self.slots.push(UnsafeCell::new(value));
        index
    }

    #[inline]
    fn truncate(&mut self, mark: usize) {
        self.slots.truncate(mark);
    }

    #[inline]
    fn slot(&self, index: usize) -> &UnsafeCell<Object> {
        &self.slots[index]
    }

    #[inline]
    fn set(&self, index: usize, value: Object) {
        unsafe { *self.slot(index).get() = value };
    }

    /// Visit every active native root while its mutator is parked.
    ///
    /// # Safety
    ///
    /// The owning mutator must remain parked for the entire scan.
    pub(crate) unsafe fn scan(&self, visitor: &mut dyn StackRootVisitor) {
        for slot in &self.slots {
            visitor.visit_tagged_slot(Address::from_ptr(slot.get()));
        }
    }
}

/// Stack-local cursor and safepoint token for native Rust code.
///
/// `mark` is the native root-stack length on entry. `size` is the current
/// length after roots created by this activation have been appended.
pub struct GcContext {
    stack: NonNull<NativeRootStack>,
    mark: usize,
    size: usize,
}

impl GcContext {
    /// Enter a native root-stack activation on the current mutator.
    #[inline]
    pub fn new() -> Self {
        let thread = crate::gc::current_thread();
        let stack = NonNull::new(thread.native_roots.get())
            .expect("mutator native root stack must have a stable address");
        let size = unsafe { stack.as_ref().len() };

        Self {
            stack,
            mark: size,
            size,
        }
    }

    #[inline]
    fn stack(&self) -> &NativeRootStack {
        unsafe { self.stack.as_ref() }
    }

    #[inline]
    fn stack_mut(&mut self) -> &mut NativeRootStack {
        unsafe { self.stack.as_mut() }
    }

    #[inline]
    fn assert_current(&self) {
        assert_eq!(
            self.stack().len(),
            self.size,
            "only the innermost GcContext may manipulate native roots",
        );
    }

    #[inline]
    fn push_object(&mut self, value: Object) -> usize {
        self.assert_current();
        let index = self.stack_mut().push(value);
        self.size += 1;
        index
    }

    /// Append one tagged value to the current mutator's native root stack.
    #[inline]
    pub fn root(&mut self, value: Object) -> Rooted {
        Rooted::from_index(self.push_object(value))
    }

    /// Append one typed heap reference as a tagged native root.
    #[inline]
    pub fn root_gc<T: HeapObject>(&mut self, value: Gc<T>) -> GcRooted<T> {
        let raw = value.as_untagged_ptr().as_ptr() as usize as u64 | T::PRIMARY_TAG as u64;
        GcRooted::from_index(self.push_object(Object::from_raw(raw)))
    }

    /// Copy a contiguous argument slice into the native root stack.
    pub fn root_slice(&mut self, values: &[Object]) -> RootedSlice {
        self.assert_current();
        let start = self.size;

        for value in values.iter().copied() {
            self.stack_mut().push(value);
            self.size += 1;
        }

        RootedSlice::new(start, values.len())
    }

    /// Poll the current mutator's safepoint flag.
    ///
    /// Moving GC may update published root slots while this call blocks.
    #[inline]
    pub fn safepoint(&mut self) {
        self.assert_current();
        crate::gc::current_thread().safepoint_poll();
    }

    /// Allocate an unrooted handle. Root it explicitly before another safepoint.
    ///
    /// Constructor inputs must be rooted; a prebuilt cons is rejected:
    /// ```compile_fail
    /// use torrent::{core::cons::Cons, gc::GcContext};
    /// fn prebuilt(ctx: &mut GcContext, payload: Cons) {
    ///     ctx.alloc::<Cons>(payload);
    /// }
    /// ```
    pub fn alloc<T: HeapObject>(&mut self, init: T::Init<'_>) -> Gc<T> {
        self.assert_current();
        let size = T::size_from_init(&init);
        let thread = crate::gc::current_thread();
        #[cfg(test)]
        crate::gc::test_support::collect_before_allocation();
        let start = thread.alloc(size);
        // Allocation may move constructor inputs. Reload them from their roots
        // now; no safepoint is allowed until initialization and post_alloc finish.
        let payload = unsafe { T::init_fn(init, &*self) };
        assert_eq!(payload.size(), size, "constructor allocation size mismatch");
        unsafe { T::init(start, payload) };
        let object = unsafe { T::object_ref_from_start(start) };
        crate::gc::post_alloc_with_semantics(
            thread.mutator(),
            object,
            size,
            mmtk::AllocationSemantics::Default,
        );
        Gc::from_object_ref(object)
    }
}

impl Drop for GcContext {
    fn drop(&mut self) {
        self.assert_current();
        let mark = self.mark;
        self.stack_mut().truncate(mark);
        self.size = mark;
    }
}

/// Index of one active value in the current mutator's native root stack.
///
/// Constructors are private so safe runtime code can only obtain handles from
/// a live `GcContext`. The procedural wrappers keep handles inside that
/// context's call frame.
pub struct Rooted<T = Object> {
    index: usize,
    _ty: PhantomData<fn() -> T>,
    _not_send: PhantomData<Rc<()>>,
}

pub type GcRooted<T> = Rooted<Gc<T>>;

impl<T> Rooted<T> {
    #[inline]
    fn from_index(index: usize) -> Self {
        Self {
            index,
            _ty: PhantomData,
            _not_send: PhantomData,
        }
    }
}

impl<T> fmt::Debug for Rooted<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Rooted").field(&self.index).finish()
    }
}

impl Rooted {
    #[inline]
    pub fn get<'gc>(&self, ctx: &'gc GcContext) -> &'gc Object {
        ctx.assert_current();
        assert!(self.index < ctx.size, "root index is outside this context");
        unsafe { &*ctx.stack().slot(self.index).get() }
    }

    #[inline]
    pub fn set(&self, value: Object, ctx: &mut GcContext) {
        ctx.assert_current();
        assert!(self.index < ctx.size, "root index is outside this context");
        ctx.stack().set(self.index, value);
    }
}

impl<T: HeapObject> GcRooted<T> {
    #[inline]
    pub fn get_gc<'gc>(&self, ctx: &'gc GcContext) -> &'gc T {
        ctx.assert_current();
        assert!(self.index < ctx.size, "root index is outside this context");
        let object = unsafe { &*ctx.stack().slot(self.index).get() };
        let object_ref = unsafe { object.object_ref_unchecked() };
        unsafe { T::data_from_object_ref(object_ref).as_ref() }
    }

    /// Snapshot the current handle; root it before retaining it across a safepoint.
    pub fn to_gc(&self, ctx: &GcContext) -> Gc<T> {
        let value = self.get_gc(ctx);
        Gc::from_object_ref(unsafe { T::object_ref_from_data(NonNull::from(value)) })
    }

    #[inline]
    pub fn set_gc(&self, value: Gc<T>, ctx: &mut GcContext) {
        ctx.assert_current();
        assert!(self.index < ctx.size, "root index is outside this context");
        let raw = value.as_untagged_ptr().as_ptr() as usize as u64 | T::PRIMARY_TAG as u64;
        ctx.stack().set(self.index, Object::from_raw(raw));
    }
}

/// Contiguous range of active native root indices.
pub struct RootedSlice {
    start: usize,
    len: usize,
    _not_send: PhantomData<Rc<()>>,
}

impl RootedSlice {
    #[inline]
    fn new(start: usize, len: usize) -> Self {
        Self {
            start,
            len,
            _not_send: PhantomData,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<Rooted> {
        (index < self.len).then(|| Rooted::from_index(self.start + index))
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = Rooted> + ExactSizeIterator + use<> {
        let end = self
            .start
            .checked_add(self.len)
            .expect("rooted slice index overflow");
        (self.start..end).map(Rooted::from_index)
    }
}

impl fmt::Debug for RootedSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootedSlice")
            .field("start", &self.start)
            .field("len", &self.len)
            .finish()
    }
}

/// JIT root frame describing one generated-code stack-map segment.
///
/// `stop_frame_pointer` is the older JIT frame boundary, allowing nested
/// `jit -> native -> jit -> native` transitions to scan only their own segment.
#[repr(C, align(16))]
pub struct JitRootFrame {
    pub prev: *mut JitRootFrame,
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
            prev: ptr::null_mut(),
            frame_pointer,
            stop_frame_pointer: ptr::null(),
            stack_pointer,
            return_pc,
            dynamic_env,
        }
    }
}

/// RAII guard for a published JIT root frame.
pub struct RootFrameGuard {
    thread: *const MutatorThread,
    expected_head: *mut JitRootFrame,
    prev: *mut JitRootFrame,
}

impl Drop for RootFrameGuard {
    fn drop(&mut self) {
        unsafe {
            let thread = &*self.thread;
            debug_assert_eq!(
                thread.jit_root_head.get(),
                self.expected_head,
                "JIT root frames must be popped in LIFO order"
            );
            thread.jit_root_head.set(self.prev);
        }
    }
}

/// Push a JIT root frame onto the current mutator's JIT root chain.
///
/// # Safety
///
/// `frame` must remain valid and unmoved until the returned guard is dropped.
/// `return_pc` must identify a known JIT safepoint when a GC scans the frame.
pub unsafe fn push_jit_root_frame(
    thread: &MutatorThread,
    frame: *mut JitRootFrame,
) -> RootFrameGuard {
    let prev = thread.jit_root_head.get();
    unsafe {
        (*frame).prev = prev;
        (*frame).stop_frame_pointer = if prev.is_null() {
            ptr::null()
        } else {
            (*prev).frame_pointer
        };
    }

    thread.jit_root_head.set(frame);

    RootFrameGuard {
        thread,
        expected_head: frame,
        prev,
    }
}

/// Scan all JIT root frames for one mutator.
///
/// # Safety
///
/// `frame` must be the head of a live JIT root-frame chain. `scan_jit_frame`
/// must understand the Cranelift stack-map convention used by every frame.
pub unsafe fn scan_jit_root_chain(
    mut frame: *mut JitRootFrame,
    visitor: &mut dyn StackRootVisitor,
    scan_jit_frame: unsafe fn(&JitRootFrame, &mut dyn StackRootVisitor),
) {
    while !frame.is_null() {
        let current = unsafe { &*frame };
        unsafe { scan_jit_frame(current, visitor) };
        unsafe { scan_dynamic_env(current.dynamic_env, visitor) };
        frame = current.prev;
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

    unsafe fn context_for(stack: &mut NativeRootStack) -> GcContext {
        let stack = NonNull::from(stack);
        let size = unsafe { stack.as_ref().len() };
        GcContext {
            stack,
            mark: size,
            size,
        }
    }

    #[test]
    fn native_root_stack_preserves_nested_indices_and_slice_order() {
        let mut stack = NativeRootStack::new();
        let mut outer = unsafe { context_for(&mut stack) };
        let first = outer.root(Object(1));
        let values = [Object(2), Object(3), Object(4)];
        let values = outer.root_slice(&values);

        assert_eq!(
            values
                .iter()
                .map(|root| root.get(&outer).raw())
                .collect::<Vec<_>>(),
            vec![2, 3, 4],
        );
        assert_eq!(
            values
                .iter()
                .rev()
                .map(|root| root.get(&outer).raw())
                .collect::<Vec<_>>(),
            vec![4, 3, 2],
        );

        for raw in 5..256 {
            outer.root(Object(raw));
        }
        assert_eq!(first.get(&outer).raw(), 1);

        let outer_size = outer.size;
        {
            let mut inner = unsafe { context_for(outer.stack.as_mut()) };
            let child = inner.root(Object(256));
            assert_eq!(child.get(&inner).raw(), 256);
        }
        assert_eq!(outer.stack().len(), outer_size);

        let mut visitor = CollectingVisitor(Vec::new());
        unsafe {
            outer.stack().scan(&mut visitor);
        }
        assert_eq!(visitor.0.len(), outer_size);

        drop(outer);
        assert_eq!(stack.len(), 0);
    }

    #[test]
    fn jit_root_chain_scans_nested_jit_and_dynamic_env() {
        #[repr(C)]
        struct DynamicEnv1 {
            header: DynamicEnv,
            bindings: [DynamicBinding; 1],
        }

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
        let mut jit_value = Object(2);
        let mut older = JitRootFrame::new(
            0x1000 as *const usize,
            ptr::null(),
            ptr::null(),
            ptr::null(),
        );
        let mut newer = JitRootFrame::new(
            0x2000 as *const usize,
            &mut jit_value as *mut Object as *const usize,
            ptr::null(),
            &mut dynamic.header,
        );
        newer.prev = &mut older;
        newer.stop_frame_pointer = older.frame_pointer;
        let mut visitor = CollectingVisitor(Vec::new());

        unsafe {
            scan_jit_root_chain(&mut newer, &mut visitor, fake_jit_scan);
        }

        assert_eq!(
            visitor.0,
            vec![
                Address::from_ptr(&mut jit_value as *mut Object),
                Address::from_ptr(&mut dynamic.bindings[0].obj as *mut Object),
                Address::from_ptr(ptr::null_mut::<Object>()),
            ]
        );
    }

    #[test]
    fn tagged_ptr_slot_updates_rooted_object_word() {
        let mut stack = NativeRootStack::new();
        let mut ctx = unsafe { context_for(&mut stack) };
        let root = ctx.root(Object(0x1000 | GcTag::CONS as u64));
        let address = Address::from_ptr(ctx.stack().slot(root.index).get());
        let slot = TaggedPtrSlot::from_address(address);
        let moved = unsafe {
            mmtk::util::ObjectReference::from_raw_address_unchecked(Address::from_usize(0x2000))
        };

        slot.store(moved);

        assert_eq!(root.get(&ctx).raw(), 0x2000 | GcTag::CONS as u64);
    }
}
