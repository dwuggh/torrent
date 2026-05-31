use mmtk::vm::ActivePlan;

use crate::thread::{MutatorThread, THREAD_MANAGER, is_mutator};
use crate::vm::MM;

pub struct VMActivePlan;

impl ActivePlan<MM> for VMActivePlan {
    fn is_mutator(tls: mmtk::util::VMThread) -> bool {
        is_mutator(tls)
    }

    fn mutator(tls: mmtk::util::VMMutatorThread) -> &'static mut mmtk::Mutator<MM> {
        let thread = unsafe { MutatorThread::from_mutator_thread(tls) };
        let mutator = thread
            .mutator_ptr()
            .expect("mutator thread has no bound MMTk mutator");
        unsafe { &mut *mutator }
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut mmtk::Mutator<MM>> + 'a> {
        let mutators = THREAD_MANAGER.bound_mutator_ptrs();
        Box::new(mutators.into_iter().map(|mutator| unsafe { &mut *mutator }))
    }

    fn number_of_mutators() -> usize {
        THREAD_MANAGER.number_of_bound_mutators()
    }
}
