use std::cell::{Cell, RefCell, UnsafeCell};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex, MutexGuard, Weak};

use mmtk::util::{Address, OpaquePointer, VMMutatorThread, VMThread, VMWorkerThread};
use mmtk::{Mutator, MutatorContext};

use crate::vm::MM;

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

#[repr(C)]
struct ThreadHeader {
    kind: ThreadKind,
    id: u32,
}

#[repr(C)]
pub struct MutatorThread {
    pub header: ThreadHeader,
    /// JIT fast path for allocation
    pub block_requested: AtomicBool,
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
struct GcWorkerThread {
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
    GcWorker(Arc<GcWorkerThread>)
}

// Thread-local handle to the registered Thread.
thread_local! {
    static TLS_THREAD: RefCell<Option<TlsThread>> = const { RefCell::new(None) };
    static TLS_THREAD_PTR: Cell<Option<NonNull<ThreadHeader>>> = const {
        Cell::new(None)
    };
}

impl MutatorThread {
    pub fn new(manager: &std::sync::Arc<ThreadManager>) -> Self {
        let id = manager.next_id();
        Self {
            header: ThreadHeader { kind: ThreadKind::Mutator, id },
            block_requested: AtomicBool::new(false),
            mutator: UnsafeCell::new(None),
            park_lock: Mutex::new(ThreadState::Mutating),
            park_cv: Condvar::new(),
            manager: Arc::downgrade(manager),
        }
    }

    fn mutator(&self) -> &mut Mutator<MM> {
        todo!()
    }

    pub fn alloc(&self, size: usize) -> Address {
        self.mutator()
            .alloc(size, 8, 0, mmtk::AllocationSemantics::Default)
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
        // gc workloads can start here

        while *state == ThreadState::Blocked {
            state = self.park_cv.wait(state).unwrap();
        }
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

impl ThreadManager {
    pub fn new() -> Self {
        Self {
            mutator_threads: Mutex::new(Vec::new()),
            gc_threads: Mutex::new(Vec::new()),
            next_thread_id: AtomicU32::new(0),
        }
    }

    pub fn request_stop_all_mutators<F>(&self, mut visitor: F)
        where F: FnMut(&'static mut Mutator<MM>)
    {
        let threads = self.mutator_threads.lock().unwrap();


        for t in threads.iter() {
            t.request_block();
        }

        for t in threads.iter() {
            t.wait_until_blocked();
        }

        // NOTE in future, flush tlabs here

        for t in threads.iter() {
            let mutator = unsafe {
                &mut *(t.mutator() as *mut _)
            };
            visitor(mutator)
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
        unsafe { Self::try_from_vmthread(tls) }
            .expect("VMThread does not point to a MutatorThread")
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
