use std::sync::MutexGuard;

use mmtk::vm::ActivePlan;

use crate::thread::{MutatorThread, is_mutator};
use crate::vm::MM;

pub struct VMActivePlan;

impl ActivePlan<MM> for VMActivePlan {
    fn is_mutator(tls: mmtk::util::VMThread) -> bool {
        is_mutator(tls)
    }

    fn mutator(tls: mmtk::util::VMMutatorThread) -> &'static mut mmtk::Mutator<MM> {
        todo!()
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut mmtk::Mutator<MM>> + 'a> {
        todo!()
    }

    fn number_of_mutators() -> usize {
        todo!()
    }
}
