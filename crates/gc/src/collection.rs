use mmtk::vm::Collection;

use crate::thread::{self, THREAD_MANAGER};
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
        let thread = crate::thread::MutatorThread::from_mutator_thread(tls);
        thread.block();
    }

    fn spawn_gc_thread(tls: mmtk::util::VMThread, ctx: mmtk::vm::GCThreadContext<MM>) {
        // Minimal wiring: leave unimplemented until MMTk integration is finalized.
        // GC workers will be spawned by the runtime embedding.
        // (This hook can be filled with worker.run(...) once the global MMTk is available.)
        let _ = (tls, ctx);
        todo!()
    }
}
