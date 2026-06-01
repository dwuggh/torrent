use std::cell::{Cell, RefCell, UnsafeCell};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU8, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex, Weak};
use std::thread::JoinHandle;

use mmtk::util::{
    alloc::{AllocatorSelector, BumpAllocator, ImmixAllocator},
    Address, OpaquePointer, VMMutatorThread, VMThread, VMWorkerThread,
};
use mmtk::{AllocationSemantics, Mutator, MutatorContext};

use super::{default_allocator_selector, lab::LocalAllocationBuffer, vm::MM, OBJECT_REF_OFFSET};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ThreadState {
    New,
    Mutating,
    RequestToBlock,
    Blocked,
    Immutable,
    BlockedInImmutable,
    Terminated,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ThreadKind {
    Mutator = 1,
    GcWorker = 2,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AllocFastPath {
    None = 0,
    Tlab = 1,
}

impl AllocFastPath {
    fn from_byte(value: u8) -> Self {
        match value {
            1 => Self::Tlab,
            _ => Self::None,
        }
    }
}

#[repr(C)]
pub(crate) struct ThreadHeader {
    kind: ThreadKind,
    id: u32,
}

#[repr(C)]
pub struct MutatorThread {
    pub header: ThreadHeader,
    pub block_requested: AtomicBool,
    /// JIT fast path for allocation.
    pub lab: Cell<LocalAllocationBuffer>,
    pub alloc_fastpath: AtomicU8,
    // Mutator bound to this thread (set/unset by the runtime). Owned and pinned in TLS
    // to keep a stable address across its lifetime.
    pub(crate) mutator: UnsafeCell<Option<Box<Mutator<MM>>>>,
    // Parking primitives for this thread.
    park_lock: Mutex<ThreadState>,
    park_cv: Condvar,
    // Handle to the thread manager owning this thread.
    manager: std::sync::Weak<ThreadManager>,
}

unsafe impl Send for MutatorThread {}
unsafe impl Sync for MutatorThread {}

#[repr(C)]
pub(crate) struct GcWorkerThread {
    header: ThreadHeader,
    manager: Weak<ThreadManager>,
}

/// Manager that owns the registry of GC-managed threads and provides
/// stop-the-world orchestration. Modeled after RSGC's ThreadManager but
/// tailored to this minimal STW design.
pub struct ThreadManager {
    pub mutator_threads: Mutex<Vec<Arc<MutatorThread>>>,
    pub gc_threads: Mutex<Vec<Arc<GcWorkerThread>>>,
    next_thread_id: AtomicU32,
}

pub static THREAD_MANAGER: LazyLock<Arc<ThreadManager>> =
    LazyLock::new(|| Arc::new(ThreadManager::new()));

#[derive(Clone)]
enum TlsThread {
    Mutator(Arc<MutatorThread>),
    GcWorker(Arc<GcWorkerThread>),
}

// Thread-local handle to the registered Thread.
thread_local! {
    static TLS_THREAD: RefCell<Option<TlsThread>> = const { RefCell::new(None) };
    static TLS_THREAD_PTR: Cell<Option<NonNull<ThreadHeader>>> = const {
        Cell::new(None)
    };
}

impl MutatorThread {
    #[allow(dead_code)]
    pub const LAB_OFFSET: usize = std::mem::offset_of!(Self, lab);
    #[allow(dead_code)]
    pub const LAB_CURSOR_OFFSET: usize =
        Self::LAB_OFFSET + std::mem::offset_of!(LocalAllocationBuffer, cursor);
    #[allow(dead_code)]
    pub const LAB_LIMIT_OFFSET: usize =
        Self::LAB_OFFSET + std::mem::offset_of!(LocalAllocationBuffer, limit);
    #[allow(dead_code)]
    pub const ALLOC_FASTPATH_OFFSET: usize = std::mem::offset_of!(Self, alloc_fastpath);

    pub fn new(manager: &std::sync::Arc<ThreadManager>) -> Self {
        let id = manager.next_id();
        Self {
            header: ThreadHeader {
                kind: ThreadKind::Mutator,
                id,
            },
            block_requested: AtomicBool::new(false),
            lab: Cell::new(LocalAllocationBuffer::new()),
            alloc_fastpath: AtomicU8::new(AllocFastPath::None as u8),
            mutator: UnsafeCell::new(None),
            park_lock: Mutex::new(ThreadState::Mutating),
            park_cv: Condvar::new(),
            manager: Arc::downgrade(manager),
        }
    }

    pub fn bind_mutator(&self, mutator: Box<Mutator<MM>>, alloc_fastpath: AllocFastPath) {
        let slot = unsafe { &mut *self.mutator.get() };
        assert!(slot.is_none(), "mutator is already bound to this thread");
        self.alloc_fastpath
            .store(alloc_fastpath as u8, Ordering::Release);
        *slot = Some(mutator);
    }

    pub fn take_mutator(&self) -> Option<Box<Mutator<MM>>> {
        self.flush_tlab();
        self.alloc_fastpath
            .store(AllocFastPath::None as u8, Ordering::Release);
        unsafe { &mut *self.mutator.get() }.take()
    }

    pub(crate) fn mutator(&self) -> &mut Mutator<MM> {
        // The VM binding creates mutable mutator references only for the current
        // thread or after stop-the-world has parked mutators.
        let slot = unsafe { &mut *self.mutator.get() };
        slot.as_deref_mut()
            .expect("mutator thread has no bound MMTk mutator")
    }

    pub fn guard_mutator(&self) -> Option<MutatorGuard<'_>> {
        unsafe { &*self.mutator.get() }
            .as_ref()
            .map(|_| MutatorGuard { thread: self })
    }

    pub(crate) fn mutator_ptr(&self) -> Option<*mut Mutator<MM>> {
        unsafe { &mut *self.mutator.get() }
            .as_deref_mut()
            .map(|mutator| mutator as *mut Mutator<MM>)
    }

    pub fn alloc(&self, size: usize) -> Address {
        if AllocFastPath::from_byte(self.alloc_fastpath.load(Ordering::Acquire))
            == AllocFastPath::Tlab
        {
            let mut lab = self.lab.get();
            if let Some(start) = lab.allocate(size, 8, OBJECT_REF_OFFSET) {
                self.lab.set(lab);
                return start;
            }

            return self.alloc_slow_default(size);
        }

        self.alloc_default(size)
    }

    fn alloc_default(&self, size: usize) -> Address {
        self.mutator().alloc(
            size,
            8,
            OBJECT_REF_OFFSET,
            AllocationSemantics::Default,
        )
    }

    fn alloc_slow_default(&self, size: usize) -> Address {
        self.flush_tlab();
        let start = self.alloc_default(size);
        self.refill_tlab();
        start
    }

    pub(crate) fn flush_tlab(&self) {
        let mut lab = self.lab.get();
        let (cursor, limit) = lab.take();
        self.lab.set(lab);
        if cursor.is_zero() {
            debug_assert!(limit.is_zero());
            return;
        }

        match default_allocator_selector() {
            selector @ AllocatorSelector::Immix(_) => unsafe {
                self.mutator()
                    .allocator_impl_mut::<ImmixAllocator<MM>>(selector)
                    .bump_pointer
                    .reset(cursor, limit);
            },
            selector @ AllocatorSelector::BumpPointer(_) => unsafe {
                self.mutator()
                    .allocator_impl_mut::<BumpAllocator<MM>>(selector)
                    .bump_pointer
                    .reset(cursor, limit);
            },
            _ => {}
        }
    }

    fn refill_tlab(&self) {
        let Some((cursor, limit)) = (match default_allocator_selector() {
            selector @ AllocatorSelector::Immix(_) => unsafe {
                let bump_pointer = &self
                    .mutator()
                    .allocator_impl_mut::<ImmixAllocator<MM>>(selector)
                    .bump_pointer;
                Some((bump_pointer.cursor, bump_pointer.limit))
            },
            selector @ AllocatorSelector::BumpPointer(_) => unsafe {
                let bump_pointer = &self
                    .mutator()
                    .allocator_impl_mut::<BumpAllocator<MM>>(selector)
                    .bump_pointer;
                Some((bump_pointer.cursor, bump_pointer.limit))
            },
            _ => None,
        }) else {
            return;
        };

        let mut lab = self.lab.get();
        lab.rebind(cursor, limit);
        self.lab.set(lab);
    }

    pub fn safepoint_poll(&self) {
        if !self.block_requested.load(Ordering::Acquire) {
            return;
        }
        self.safepoint_poll_slow();
    }

    pub fn safepoint_poll_slow(&self) {
        let mut state = self.park_lock.lock().unwrap();
        if *state != ThreadState::RequestToBlock {
            // false positive: mm may cancel block request, so directly return
            self.block_requested.store(false, Ordering::Release);
            return;
        }

        // maybe process roots

        *state = ThreadState::Blocked;
        self.block_requested.store(false, Ordering::Release);
        self.park_cv.notify_all();
        // gc workloads can start here

        while *state == ThreadState::Blocked {
            state = self.park_cv.wait(state).unwrap();
        }
    }

    pub fn block(&self) {
        let mut state = self.park_lock.lock().unwrap();
        match *state {
            ThreadState::Mutating | ThreadState::RequestToBlock => {
                // process local handles
                *state = ThreadState::Blocked;
                self.block_requested.store(false, Ordering::Release);
                self.park_cv.notify_all();
            }
            ThreadState::Immutable | ThreadState::BlockedInImmutable => {
                panic!(
                    "block_for_gc called while thread is not mutating: {:?}",
                    *state
                );
            }
            ThreadState::New | ThreadState::Terminated => {
                panic!("block_for_gc called on non-running thread: {:?}", *state);
            }
            ThreadState::Blocked => {}
        }

        while *state == ThreadState::Blocked {
            state = self.park_cv.wait(state).unwrap();
        }

        // resumed
        // reload roots
    }

    // called from gc thread
    fn request_block(&self) {
        let mut state = self.park_lock.lock().unwrap();
        match *state {
            ThreadState::Mutating => {
                *state = ThreadState::RequestToBlock;
                self.block_requested.store(true, Ordering::Release);
            }
            ThreadState::Immutable => {
                *state = ThreadState::BlockedInImmutable;
                self.block_requested.store(false, Ordering::Release);
                // wake it up, so it can continue its native job.
                self.park_cv.notify_all();
            }
            _ => {}
        }
    }

    fn wait_until_blocked(&self) {
        let mut state = self.park_lock.lock().unwrap();
        while !matches!(
            *state,
            ThreadState::Blocked
                | ThreadState::BlockedInImmutable
                | ThreadState::New
                | ThreadState::Terminated
        ) {
            state = self.park_cv.wait(state).unwrap();
        }
    }

    fn resume(&self) {
        let mut state = self.park_lock.lock().unwrap();
        match *state {
            ThreadState::Blocked => {
                *state = ThreadState::Mutating;
                self.block_requested.store(false, Ordering::Release);
                self.park_cv.notify_all();
            }
            ThreadState::BlockedInImmutable => {
                *state = ThreadState::Immutable;
                self.block_requested.store(false, Ordering::Release);
                self.park_cv.notify_all();
            }
            _ => {}
        }
    }

    fn enter_native(&self) {
        loop {
            let mut state = self.park_lock.lock().unwrap();
            match *state {
                ThreadState::Mutating => {
                    *state = ThreadState::Immutable;
                    return;
                }
                ThreadState::RequestToBlock => {
                    *state = ThreadState::BlockedInImmutable;
                    // TODO is this needed?
                    self.park_cv.notify_all();
                }

                _ => {
                    // TODO error here
                }
            }
            self.block_requested.store(false, Ordering::Release);
        }
    }

    fn leave_native(&self) {
        let mut state = self.park_lock.lock().unwrap();

        // If requested GC while in native code, wait until it finish
        while *state == ThreadState::BlockedInImmutable {
            state = self.park_cv.wait(state).unwrap();
        }

        assert_eq!(*state, ThreadState::Immutable);

        *state = ThreadState::Mutating;
    }
}

pub struct MutatorGuard<'a> {
    thread: &'a MutatorThread,
}

impl Deref for MutatorGuard<'_> {
    type Target = Mutator<MM>;

    fn deref(&self) -> &Self::Target {
        let ptr = self.thread.mutator() as *mut Mutator<MM> as *const Mutator<MM>;
        unsafe { &*ptr }
    }
}

impl DerefMut for MutatorGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.thread.mutator()
    }
}

impl ThreadManager {
    pub fn new() -> Self {
        Self {
            mutator_threads: Mutex::new(Vec::new()),
            gc_threads: Mutex::new(Vec::new()),
            next_thread_id: AtomicU32::new(0),
        }
    }

    pub fn request_stop_all_mutators<F>(&self, mut visitor: F)
    where
        F: FnMut(&'static mut Mutator<MM>),
    {
        let threads = self.mutator_threads.lock().unwrap();

        for t in threads.iter() {
            t.request_block();
        }

        for t in threads.iter() {
            t.wait_until_blocked();
        }

        for t in threads.iter() {
            t.flush_tlab();
        }

        for t in threads.iter() {
            if let Some(mutator) = t.mutator_ptr() {
                visitor(unsafe { &mut *mutator })
            }
        }

        // let active = threads.len();
        // while self.parked.load(Ordering::SeqCst) < active {
        //     threads = self.cv_join.wait(threads).unwrap();
        // }
    }

    pub fn resume_all_mutators(&self) {
        let threads = self.mutator_threads.lock().unwrap().clone();
        for thread in &threads {
            thread.resume();
        }
    }

    fn next_id(&self) -> u32 {
        self.next_thread_id.fetch_add(1, Ordering::Relaxed)
    }

    pub fn add_or_get_current_thread(self: &Arc<Self>) -> Arc<MutatorThread> {
        TLS_THREAD.with(|tls| {
            if let Some(thread) = tls.borrow().clone() {
                return match thread {
                    TlsThread::Mutator(thread) => thread,
                    TlsThread::GcWorker(_) => {
                        panic!("current thread is already registered as a GC worker")
                    }
                };
            }

            let thread = Arc::new(MutatorThread::new(self));
            TLS_THREAD_PTR.with(|ptr| ptr.set(NonNull::new(&thread.header as *const _ as *mut _)));
            *tls.borrow_mut() = Some(TlsThread::Mutator(thread.clone()));
            self.mutator_threads.lock().unwrap().push(thread.clone());
            thread
        })
    }

    pub(crate) fn add_current_gc_thread(self: &Arc<Self>) -> Arc<GcWorkerThread> {
        TLS_THREAD.with(|tls| {
            if let Some(thread) = tls.borrow().clone() {
                return match thread {
                    TlsThread::GcWorker(thread) => thread,
                    TlsThread::Mutator(_) => {
                        panic!("current thread is already registered as a mutator")
                    }
                };
            }

            let thread = Arc::new(GcWorkerThread::new(self));
            TLS_THREAD_PTR.with(|ptr| ptr.set(NonNull::new(&thread.header as *const _ as *mut _)));
            *tls.borrow_mut() = Some(TlsThread::GcWorker(thread.clone()));
            self.gc_threads.lock().unwrap().push(thread.clone());
            thread
        })
    }

    pub(crate) fn bound_mutator_ptrs(&self) -> Vec<*mut Mutator<MM>> {
        self.mutator_threads
            .lock()
            .unwrap()
            .iter()
            .filter_map(|thread| thread.mutator_ptr())
            .collect()
    }

    pub(crate) fn number_of_bound_mutators(&self) -> usize {
        self.mutator_threads
            .lock()
            .unwrap()
            .iter()
            .filter(|thread| thread.mutator_ptr().is_some())
            .count()
    }
}

pub fn current_thread() -> Arc<MutatorThread> {
    TLS_THREAD.with(|tls| match tls.borrow().clone() {
        Some(TlsThread::Mutator(thread)) => thread,
        Some(TlsThread::GcWorker(_)) => panic!("current thread is a GC worker, not a mutator"),
        None => panic!("current thread is not registered with the GC thread manager"),
    })
}

pub fn is_mutator(tls: VMThread) -> bool {
    unsafe {
        let header = header_from_vmthread(tls);
        header.kind == ThreadKind::Mutator
    }
}

#[inline]
fn vmthread_from_header(header: &ThreadHeader) -> VMThread {
    VMThread(OpaquePointer::from_address(Address::from_ref(header)))
}

#[inline]
unsafe fn header_from_vmthread<'a>(tls: VMThread) -> &'a ThreadHeader {
    unsafe { tls.0.to_address().as_ref::<ThreadHeader>() }
}

#[inline]
pub unsafe fn kind_from_vmthread(tls: VMThread) -> ThreadKind {
    unsafe { header_from_vmthread(tls).kind }
}

impl MutatorThread {
    /// Create a VMThread opaque handle pointing to this thread's common header.
    ///
    /// This does not require `&'static self`. The caller/runtime must ensure
    /// the object remains alive while MMTk may use the pointer.
    #[inline]
    pub fn to_vmthread(&self) -> VMThread {
        vmthread_from_header(&self.header)
    }

    /// Create a VMMutatorThread opaque handle from this mutator thread.
    #[inline]
    pub fn to_mutator_thread(&self) -> VMMutatorThread {
        VMMutatorThread(self.to_vmthread())
    }

    /// Try to recover a mutator thread from a generic VMThread.
    ///
    /// Safety:
    /// - `tls` must have been created from a live VM thread object owned by this runtime.
    /// - The object must remain alive for the returned reference.
    /// - No mutable aliasing violation may be created by using the returned reference.
    #[inline]
    pub unsafe fn try_from_vmthread<'a>(tls: VMThread) -> Option<&'a MutatorThread> {
        let header = unsafe { header_from_vmthread(tls) };

        if header.kind != ThreadKind::Mutator {
            return None;
        }

        let ptr = header as *const ThreadHeader as *const MutatorThread;
        Some(unsafe { &*ptr })
    }

    /// Recover a mutator thread from a VMThread, panicking if the kind is wrong.
    ///
    /// Safety: same as `try_from_vmthread`.
    #[inline]
    pub unsafe fn from_vmthread<'a>(tls: VMThread) -> &'a MutatorThread {
        unsafe { Self::try_from_vmthread(tls) }.expect("VMThread does not point to a MutatorThread")
    }

    /// Recover a mutator thread from a VMMutatorThread.
    ///
    /// Safety: same as `from_vmthread`.
    #[inline]
    pub unsafe fn from_mutator_thread<'a>(tls: VMMutatorThread) -> &'a MutatorThread {
        unsafe { Self::from_vmthread(tls.0) }
    }
}

impl GcWorkerThread {
    fn new(manager: &std::sync::Arc<ThreadManager>) -> Self {
        let id = manager.next_id();
        Self {
            header: ThreadHeader {
                kind: ThreadKind::GcWorker,
                id,
            },
            manager: Arc::downgrade(manager),
        }
    }

    /// Create a VMThread opaque handle pointing to this GC worker's common header.
    #[inline]
    pub fn to_vmthread(&self) -> VMThread {
        vmthread_from_header(&self.header)
    }

    /// Create a VMWorkerThread opaque handle from this GC worker thread.
    #[inline]
    pub fn to_worker_thread(&self) -> VMWorkerThread {
        VMWorkerThread(self.to_vmthread())
    }

    /// Try to recover a GC worker thread from a generic VMThread.
    ///
    /// Safety:
    /// - `tls` must have been created from a live VM thread object owned by this runtime.
    /// - The object must remain alive for the returned reference.
    #[inline]
    pub unsafe fn try_from_vmthread<'a>(tls: VMThread) -> Option<&'a GcWorkerThread> {
        let header = unsafe { header_from_vmthread(tls) };

        if header.kind != ThreadKind::GcWorker {
            return None;
        }

        let ptr = header as *const ThreadHeader as *const GcWorkerThread;
        Some(unsafe { &*ptr })
    }

    /// Recover a GC worker thread from a VMThread, panicking if the kind is wrong.
    ///
    /// Safety: same as `try_from_vmthread`.
    #[inline]
    pub unsafe fn from_vmthread<'a>(tls: VMThread) -> &'a GcWorkerThread {
        unsafe { Self::try_from_vmthread(tls) }
            .expect("VMThread does not point to a GcWorkerThread")
    }

    /// Recover a GC worker thread from a VMWorkerThread.
    ///
    /// Safety: same as `from_vmthread`.
    #[inline]
    pub unsafe fn from_worker_thread<'a>(tls: VMWorkerThread) -> &'a GcWorkerThread {
        unsafe { Self::from_vmthread(tls.0) }
    }
}

pub(crate) fn spawn_gc_worker(
    worker: Box<mmtk::scheduler::GCWorker<MM>>,
) -> std::io::Result<JoinHandle<()>> {
    let mmtk = worker.mmtk;
    std::thread::Builder::new()
        .name("mmtk-gc-worker".to_string())
        .spawn(move || {
            let thread = THREAD_MANAGER.add_current_gc_thread();
            mmtk::memory_manager::start_worker(mmtk, thread.to_worker_thread(), worker);
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opaque_thread_header_preserves_kind() {
        let manager = Arc::new(ThreadManager::new());

        let mutator = MutatorThread::new(&manager);
        assert!(is_mutator(mutator.to_vmthread()));
        assert_eq!(
            unsafe { kind_from_vmthread(mutator.to_vmthread()) },
            ThreadKind::Mutator
        );

        let worker = GcWorkerThread::new(&manager);
        assert!(!is_mutator(worker.to_vmthread()));
        assert_eq!(
            unsafe { kind_from_vmthread(worker.to_vmthread()) },
            ThreadKind::GcWorker
        );
    }
}
