use super::{
    active_plan::VMActivePlan, collection::VMCollection, object_model::VMObjectModel,
    reference_glue::VMReferenceGlue, scanning::VMScanning, TaggedPtrSlot,
};

pub struct MM;

impl Default for MM {
    fn default() -> Self {
        Self
    }
}

impl mmtk::vm::VMBinding for MM {
    type VMObjectModel = VMObjectModel;

    type VMScanning = VMScanning;

    type VMCollection = VMCollection;

    type VMActivePlan = VMActivePlan;

    type VMReferenceGlue = VMReferenceGlue;

    type VMSlot = TaggedPtrSlot;

    type VMMemorySlice = mmtk::vm::slot::UnimplementedMemorySlice<TaggedPtrSlot>;
}
