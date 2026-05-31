use mmtk::vm::Collection;

use crate::thread::{THREAD_MANAGER, spawn_gc_worker};
use crate::vm::MM;

pub struct VMCollection;

impl Collection<MM> for VMCollection {
    fn stop_all_mutators<F>(_tls: mmtk::util::VMWorkerThread, mutator_visitor: F)
    where
        F: FnMut(&'static mut mmtk::Mutator<MM>),
    {
        THREAD_MANAGER.request_stop_all_mutators(mutator_visitor);
    }

    fn resume_mutators(_tls: mmtk::util::VMWorkerThread) {
        THREAD_MANAGER.resume_all_mutators();
    }

    fn block_for_gc(tls: mmtk::util::VMMutatorThread) {
        // Park this mutator if a GC request is in flight.
        let thread = unsafe { crate::thread::MutatorThread::from_mutator_thread(tls) };
        thread.block();
    }

    fn spawn_gc_thread(_tls: mmtk::util::VMThread, ctx: mmtk::vm::GCThreadContext<MM>) {
        match ctx {
            mmtk::vm::GCThreadContext::Worker(worker) => {
                spawn_gc_worker(worker).expect("failed to spawn MMTk GC worker thread");
            }
        }
    }
}
