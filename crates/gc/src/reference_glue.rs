use mmtk::{util::ObjectReference, vm::ReferenceGlue};

use crate::vm::MM;

pub struct VMReferenceGlue;

impl ReferenceGlue<MM> for VMReferenceGlue {
    type FinalizableType = ObjectReference;

    fn clear_referent(new_reference: mmtk::util::ObjectReference) {
        todo!()
    }

    fn get_referent(object: mmtk::util::ObjectReference) -> Option<mmtk::util::ObjectReference> {
        todo!()
    }

    fn set_referent(reff: mmtk::util::ObjectReference, referent: mmtk::util::ObjectReference) {
        todo!()
    }

    fn enqueue_references(
        references: &[mmtk::util::ObjectReference],
        tls: mmtk::util::VMWorkerThread,
    ) {
        todo!()
    }
}
