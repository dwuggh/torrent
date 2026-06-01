use std::sync::OnceLock;

use mmtk::util::{Address, VMMutatorThread};
use mmtk::vm::Scanning;

use super::{
    metadata_for_object, mmtk_instance, vm::MM, TaggedPtrSlot, Visitor, VisitorImpl,
};

pub trait StackMapProvider: Send + Sync {
    /// Visit precise stack roots for the given mutator thread.
    ///
    /// Providers must report addresses of machine words that hold Torrent
    /// tagged `Object` values. MMTk will load and update those words through
    /// `TaggedPtrSlot` if the referenced object moves.
    fn visit_stack_roots(&self, tls: VMMutatorThread, visitor: &mut dyn StackRootVisitor);
}

pub trait StackRootVisitor {
    /// Visit the address of a stack slot containing one raw tagged `Object`.
    fn visit_tagged_slot(&mut self, slot: Address);
}

static STACKMAP_PROVIDER: OnceLock<&'static dyn StackMapProvider> = OnceLock::new();

pub fn set_stackmap_provider(p: &'static dyn StackMapProvider) {
    let _ = STACKMAP_PROVIDER.set(p);
}

pub struct VMScanning;

struct SlotVisitorAdapter<'a, SV: mmtk::vm::SlotVisitor<TaggedPtrSlot>> {
    slot_visitor: &'a mut SV,
}

impl<SV: mmtk::vm::SlotVisitor<TaggedPtrSlot>> VisitorImpl for SlotVisitorAdapter<'_, SV> {
    fn visit_slot_address(&mut self, address: Address) {
        self.slot_visitor
            .visit_slot(TaggedPtrSlot::from_address(address));
    }
}

impl Scanning<MM> for VMScanning {
    fn scan_object<SV: mmtk::vm::SlotVisitor<<MM as mmtk::vm::VMBinding>::VMSlot>>(
        _tls: mmtk::util::VMWorkerThread,
        object: mmtk::util::ObjectReference,
        slot_visitor: &mut SV,
    ) {
        let mut adapter = SlotVisitorAdapter::<SV> { slot_visitor };
        let mut visitor = Visitor::new(&mut adapter);
        let metadata = unsafe { metadata_for_object(object) };

        // Erased scanning goes through metadata so headered and headerless
        // layouts keep their slot-address logic in one place.
        unsafe { (metadata.trace)(object, &mut visitor) };
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: mmtk::util::VMWorkerThread) {}

    fn scan_roots_in_mutator_thread(
        _tls: mmtk::util::VMWorkerThread,
        mutator: &'static mut mmtk::Mutator<MM>,
        mut factory: impl mmtk::vm::RootsWorkFactory<<MM as mmtk::vm::VMBinding>::VMSlot>,
    ) {
        let Some(provider) = STACKMAP_PROVIDER.get().copied() else {
            return;
        };

        let mut slots: Vec<TaggedPtrSlot> = Vec::new();
        let mtls = mutator.mutator_tls;
        struct RootCollector<'a>(&'a mut Vec<TaggedPtrSlot>);

        impl StackRootVisitor for RootCollector<'_> {
            fn visit_tagged_slot(&mut self, slot: Address) {
                self.0.push(TaggedPtrSlot::from_address(slot));
            }
        }

        provider.visit_stack_roots(mtls, &mut RootCollector(&mut slots));
        factory.create_process_roots_work(slots);
    }

    fn scan_vm_specific_roots(
        _tls: mmtk::util::VMWorkerThread,
        _factory: impl mmtk::vm::RootsWorkFactory<<MM as mmtk::vm::VMBinding>::VMSlot>,
    ) {
        let generational = mmtk_instance().get_plan().generational();
        // No additional VM-specific roots in this minimal design.
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}
}
