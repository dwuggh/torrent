use mmtk::{
    util::ObjectReference,
    vm::{
        VMGlobalLogBitSpec, VMLocalForwardingBitsSpec, VMLocalForwardingPointerSpec,
        VMLocalLOSMarkNurserySpec, VMLocalMarkBitSpec,
    },
};

use crate::{
    gc::{GcHeader, GcTag, TagSpec},
    vm::MM,
};

pub struct VMObjectModel;

const OBJECT_REF_OFFSET: usize = std::mem::size_of::<usize>();
const CONS_SIZE: usize = std::mem::size_of::<u64>() * 2;

impl mmtk::vm::ObjectModel<MM> for VMObjectModel {
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();

    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        VMLocalForwardingPointerSpec::in_header(0);

    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::in_header(62);

    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec = VMLocalMarkBitSpec::side_first();

    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec =
        VMLocalLOSMarkNurserySpec::side_first();

    fn copy(
        from: mmtk::util::ObjectReference,
        semantics: mmtk::util::copy::CopySemantics,
        copy_context: &mut mmtk::util::copy::GCWorkerCopyContext<MM>,
    ) -> mmtk::util::ObjectReference {
        let bytes = Self::get_current_size(from);
        let c = copy_context.alloc_copy(from, bytes, 8, 0, semantics);
        let from_start = Self::ref_to_object_start(from);
        unsafe {
            std::ptr::copy(from_start.to_ptr::<u8>(), c.to_mut_ptr::<u8>(), bytes);
        }
        let to = unsafe { ObjectReference::from_raw_address_unchecked(c + OBJECT_REF_OFFSET) };
        copy_context.post_copy(to, bytes, semantics);
        to
    }

    fn copy_to(
        from: mmtk::util::ObjectReference,
        _to: mmtk::util::ObjectReference,
        region: mmtk::util::Address,
    ) -> mmtk::util::Address {
        let bytes = Self::get_current_size(from);
        let from_start = Self::ref_to_object_start(from);
        unsafe {
            // TODO determine will region overlap with from
            std::ptr::copy(from_start.to_ptr::<u8>(), region.to_mut_ptr::<u8>(), bytes);
        }
        region + bytes
    }

    fn get_reference_when_copied_to(
        _from: mmtk::util::ObjectReference,
        to: mmtk::util::Address,
    ) -> mmtk::util::ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(to + OBJECT_REF_OFFSET) }
    }

    fn get_current_size(object: mmtk::util::ObjectReference) -> usize {
        let first_word: u64 = unsafe { Self::ref_to_header(object).load() };
        if GcTag::is_header_word(first_word) {
            let header: &GcHeader = unsafe { Self::ref_to_header(object).as_ref() };
            header.metadata().size
        } else {
            CONS_SIZE
        }
    }

    fn get_size_when_copied(object: mmtk::util::ObjectReference) -> usize {
        Self::get_current_size(object)
    }

    fn get_align_when_copied(_object: mmtk::util::ObjectReference) -> usize {
        8
    }

    fn get_align_offset_when_copied(_object: mmtk::util::ObjectReference) -> usize {
        0
    }

    fn get_type_descriptor(_reference: mmtk::util::ObjectReference) -> &'static [i8] {
        &[]
    }

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = OBJECT_REF_OFFSET as isize;

    fn ref_to_object_start(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        object.to_raw_address().sub(OBJECT_REF_OFFSET)
    }

    fn ref_to_header(object: mmtk::util::ObjectReference) -> mmtk::util::Address {
        Self::ref_to_object_start(object)
    }

    fn dump_object(_object: mmtk::util::ObjectReference) {}
}
