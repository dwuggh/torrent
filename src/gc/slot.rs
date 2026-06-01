use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::Slot;

use super::{GcTag, TagSpec};

/// A slot for pointer-sized tagged values with a fixed low primary tag.
///
/// Encoding: value = object-reference-address | low-primary-tag. Only true
/// tagged object slots should be exposed as `TaggedPtrSlot` to MMTk.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TaggedPtrSlot {
    /// Address of the machine word that holds the tagged value.
    slot_addr: Address,
}

impl TaggedPtrSlot {
    pub fn from_address(address: Address) -> Self {
        Self { slot_addr: address }
    }

    pub fn as_address(&self) -> Address {
        self.slot_addr
    }
}

unsafe impl Send for TaggedPtrSlot {}

impl Slot for TaggedPtrSlot {
    #[inline]
    fn load(&self) -> Option<ObjectReference> {
        // Non-atomic load by request.
        let raw: usize = unsafe { self.slot_addr.load() };
        let tag = raw as u8 & GcTag::MASK;
        for t in GcTag::nonref() {
            if *t == tag {
                return None;
            }
        }
        let ptr_val = raw - tag as usize;
        let addr = unsafe { Address::from_usize(ptr_val) };
        ObjectReference::from_raw_address(addr)
    }

    #[inline]
    fn store(&self, object: ObjectReference) {
        // Preserve the current low primary tag while replacing pointer bits.
        let old_raw: usize = unsafe { self.slot_addr.load() };
        let tag = old_raw as u8 & GcTag::MASK;
        let obj_addr = object.to_raw_address().as_usize();
        let new_raw = obj_addr | tag as usize;
        unsafe { self.slot_addr.store::<usize>(new_raw) };
    }
}
