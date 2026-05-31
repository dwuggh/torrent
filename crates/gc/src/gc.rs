use std::{
    alloc::Layout,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ptr::NonNull,
    sync::{LazyLock, RwLock},
};

use mmtk::util::{Address, ObjectReference};

use crate::{Tagged, thread::current_thread};

/// The tag policy for the low bits of Torrent tagged values.
///
/// These tags are the primary tags stored in the raw Lisp `Object` word. A
/// headered object uses [`GcTag::SECONDARY`] in the raw word and stores its
/// logical type tag in [`GcHeader`].
pub trait TagSpec: Copy + Send + Sync + Debug + PartialEq + Eq + Hash {
    /// Bit mask for the primary tag stored in the low bits.
    const MASK: u8;

    /// Low-bit pattern that identifies an object header word.
    const HEADER_MARKER: u8;

    /// Tags that do not represent heap references.
    fn nonref() -> &'static [u8];

    /// Return true if `word` has the header marker in its low tag bits.
    fn is_header_word(word: u64) -> bool {
        (word as u8 & Self::MASK) == Self::HEADER_MARKER
    }
}

/// Torrent's concrete primary tag policy.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GcTag;

impl GcTag {
    /// Fixnum/immediate integer tag.
    pub const INT: u8 = 0b000;
    /// Immediate symbol tag.
    pub const SYMBOL: u8 = 0b001;
    /// Headerless cons reference tag.
    pub const CONS: u8 = 0b010;
    /// Immediate character tag.
    pub const CHAR: u8 = 0b011;
    /// Header marker stored in low bits of a [`GcHeader`] word.
    pub const HEADER_MARKER_VALUE: u8 = 0b101;
    /// Raw-word tag used by references whose logical type lives in a header.
    pub const SECONDARY: u8 = 0b110;
    /// Immediate nil tag.
    pub const NIL: u8 = 0b111;
}

impl TagSpec for GcTag {
    const MASK: u8 = 0b111;
    const HEADER_MARKER: u8 = Self::HEADER_MARKER_VALUE;

    fn nonref() -> &'static [u8] {
        &[
            Self::INT,
            Self::SYMBOL,
            Self::CHAR,
            Self::HEADER_MARKER_VALUE,
            Self::NIL,
        ]
    }
}

/// Trait for exposing child reference slots in a heap object.
///
/// # Safety
///
/// Implementations must only pass addresses of real pointer-sized tagged
/// object slots to the visitor. MMTk may load from and store to every visited
/// slot during tracing or object movement.
pub unsafe trait Trace {
    /// Visit every GC-relevant slot in `self`.
    ///
    /// # Safety
    ///
    /// The implementor must uphold the slot-address invariant described on
    /// [`Trace`]. The visitor treats each address as a mutable tagged pointer
    /// slot.
    unsafe fn trace(&self, visitor: &mut Visitor);

    /// Finalize this object before reclamation.
    ///
    /// # Safety
    ///
    /// The object must still be a valid initialized value, and finalization
    /// must not expose invalid slots to the collector.
    unsafe fn finalize(&mut self) {}
}

/// Backend used by [`Visitor`] to hand slots to MMTk.
pub trait VisitorImpl {
    /// Visit the address of a pointer-sized tagged object slot.
    fn visit_slot_address(&mut self, address: Address);
}

/// Typed facade used by `Trace` implementations to report child slots.
pub struct Visitor<'a>(&'a mut dyn VisitorImpl);

impl<'a> Visitor<'a> {
    /// Create a visitor from an MMTk slot-adapter implementation.
    pub fn new(visitor: &'a mut dyn VisitorImpl) -> Self {
        Self(visitor)
    }

    /// Visit a raw slot address.
    ///
    /// The caller must ensure `address` points at a pointer-sized tagged value
    /// that MMTk is allowed to rewrite if the referenced object moves.
    pub fn visit_slot_address(&mut self, address: Address) {
        self.0.visit_slot_address(address);
    }
}

/// Erased metadata used when MMTk gives us only an [`ObjectReference`].
///
/// Runtime code registers one entry per logical heap object tag with
/// [`register_metadata`]. The GC crate keeps the table erased so it does not
/// need to know the runtime's concrete object types.
#[derive(Debug, Clone, Copy)]
pub struct MetaData {
    /// Allocation layout for objects with this logical tag.
    pub layout: Layout,
    /// Trace thunk for an erased object reference.
    pub trace: unsafe fn(object: ObjectReference, visitor: &mut Visitor),
    /// Finalization thunk for an erased object reference.
    pub finalize: unsafe fn(object: ObjectReference),
    /// Dynamic object-size thunk used by MMTk copy/size callbacks.
    pub size: unsafe fn(object: ObjectReference) -> usize,
}

static VTABLE: LazyLock<RwLock<Vec<Option<MetaData>>>> =
    LazyLock::new(|| RwLock::new(vec![None; 256]));

/// Register erased metadata for `tag`.
///
/// This is intentionally separate from [`Tagged`] so the GC crate can stay
/// independent of the runtime crate that owns the actual Lisp object types.
pub fn register_metadata(tag: u8, metadata: MetaData) {
    let mut table = VTABLE.write().expect("metadata table lock poisoned");
    table[tag as usize] = Some(metadata);
}

/// Return metadata for a logical object tag.
pub fn get_metadata_for_tag(tag: u8) -> Option<MetaData> {
    let table = VTABLE.read().expect("metadata table lock poisoned");
    table.get(tag as usize).and_then(|metadata| *metadata)
}

/// Return metadata for an erased object reference.
///
/// Headered objects are identified by the header marker one word before the
/// object reference. Headerless reference objects currently map to the cons
/// logical tag; that metadata must be registered like every other heap type.
///
/// # Safety
///
/// `object` must be an object reference produced by this VM binding.
pub unsafe fn metadata_for_object(object: ObjectReference) -> MetaData {
    let start = object.to_raw_address().sub(crate::OBJECT_REF_OFFSET);
    let first_word: u64 = unsafe { start.load() };

    let tag = if GcHeader::is_header_word(first_word) {
        let header = unsafe { start.as_ref::<GcHeader>() };
        header.tag()
    } else {
        GcTag::CONS
    };

    get_metadata_for_tag(tag).expect("no metadata for object tag")
}

/// Header word stored at the allocation start of secondary objects.
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct GcHeader {
    /// Encoded header payload.
    ///
    /// The low three bits are the header marker. The next three bits are the
    /// logical object tag. MMTk forwarding bits live in side metadata so they
    /// cannot corrupt the runtime tag bits while an object is being forwarded.
    data: u64,
}

impl GcHeader {
    /// Create a header for a logical object tag.
    pub fn new(tag: u8) -> Self {
        debug_assert_eq!(
            tag & !GcTag::MASK,
            0,
            "only three-bit logical tags fit in GcHeader"
        );
        Self {
            data: ((tag & GcTag::MASK) as u64) << 3 | GcTag::HEADER_MARKER as u64,
        }
    }

    /// Return true if `word` has the header marker in its low tag bits.
    pub fn is_header_word(word: u64) -> bool {
        GcTag::is_header_word(word)
    }

    /// Extract the logical object tag from the header.
    ///
    /// The shift removes the low header-marker bits. It is not related to
    /// endian order. We mask after shifting so unrelated higher header bits do
    /// not leak into the tag.
    pub fn tag(self) -> u8 {
        debug_assert!(Self::is_header_word(self.data));
        ((self.data >> 3) & GcTag::MASK as u64) as u8
    }

    /// Return erased metadata for this header's logical tag.
    pub fn metadata(&self) -> MetaData {
        get_metadata_for_tag(self.tag()).expect("no metadata for tag")
    }
}

/// Raw tagged Lisp object word.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub struct Object(u64);

impl Object {
    /// Create an object from a raw tagged word.
    pub fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    /// Return the raw tagged object word.
    pub fn raw(self) -> u64 {
        self.0
    }

    /// Return the low primary tag bits.
    pub fn word_tag(self) -> u8 {
        self.0 as u8 & GcTag::MASK
    }

    /// Return the untagged object-reference pointer bits.
    pub fn untagged_ptr(self) -> Option<NonNull<()>> {
        let raw = self.0 & !(GcTag::MASK as u64);
        NonNull::new(raw as *mut ())
    }
}

/// Convert a raw Lisp [`Object`] into a typed value.
///
/// This trait is the language-level tagging API. Heap implementations should
/// delegate to [`Gc::from_object`] so pointer layout stays in [`HeapObject`].
pub trait Untag: Sized {
    /// The typed value produced by untagging.
    type Target;

    /// Try to decode `object` as this type.
    fn untag(object: Object) -> Option<Self::Target>;
}

/// Convert a typed value into a raw Lisp [`Object`].
///
/// This is separate from [`HeapObject`] because immediate values and heap
/// references both participate in Lisp object tagging.
pub trait Tag {
    /// Encode `self` as a raw tagged Lisp object.
    fn tag(self) -> Object;
}

/// Typed handle for a logical heap object.
///
/// `ptr` is the same address as a raw [`Object`] after clearing the low tag
/// bits. It is also the MMTk [`ObjectReference`] address. It may be an interior
/// pointer, so it must not be interpreted directly as `*mut T`.
pub struct Gc<T: HeapObject> {
    /// Untagged Lisp object pointer / MMTk object-reference address.
    ptr: NonNull<()>,
    /// Keeps the logical heap value type attached to this opaque pointer.
    _marker: PhantomData<T>,
}

impl<T: HeapObject> Gc<T> {
    /// Allocate and initialize a new GC-managed value.
    pub fn new(value: T) -> Self {
        let thread = current_thread();
        let start = thread.alloc(T::layout().size());
        unsafe { T::init(start, value) };
        let object = unsafe { T::object_ref_from_start(start) };
        Self::from_object_ref(object)
    }

    /// Create a typed handle from an MMTk object reference.
    pub fn from_object_ref(object: ObjectReference) -> Self {
        let ptr = NonNull::new(object.to_raw_address().to_mut_ptr::<()>())
            .expect("MMTk object reference must not be null");
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Try to create a typed handle from a raw tagged Lisp object.
    pub fn from_object(object: Object) -> Option<Self> {
        if object.word_tag() != T::PRIMARY_TAG {
            return None;
        }

        Some(Self {
            ptr: object.untagged_ptr()?,
            _marker: PhantomData,
        })
    }

    /// Return this handle as an MMTk object reference.
    pub fn object_ref(&self) -> ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(Address::from_ptr(self.ptr.as_ptr())) }
    }

    /// Convert this handle to a raw tagged Lisp object word.
    pub fn to_object(&self) -> Object {
        let raw = self.ptr.as_ptr() as usize as u64;
        Object(raw | T::PRIMARY_TAG as u64)
    }

    /// Borrow the logical heap value.
    pub fn as_ref(&self) -> &T {
        unsafe { T::data_from_object_ref(self.object_ref()).as_ref() }
    }

    /// Mutably borrow the logical heap value.
    pub fn as_mut(&mut self) -> &mut T {
        unsafe { T::data_from_object_ref(self.object_ref()).as_mut() }
    }
}

impl<T: HeapObject> Untag for Gc<T> {
    type Target = Self;

    fn untag(object: Object) -> Option<Self::Target> {
        Self::from_object(object)
    }
}

impl<T: HeapObject> Tag for Gc<T> {
    fn tag(self) -> Object {
        self.to_object()
    }
}

/// Physical representation for a headered logical value.
#[repr(C)]
pub struct Headered<T: Tagged> {
    /// Header word used for erased metadata lookup.
    header: GcHeader,
    /// Logical heap value stored after the header.
    data: T,
}

/// Contract between a logical heap type and its physical heap layout.
///
/// # Safety
///
/// Implementations must convert between allocation start, MMTk object
/// reference, and typed logical value pointer without changing the object
/// identity. MMTk will use these conversions while tracing and copying.
pub unsafe trait HeapObject: Tagged + Trace + Sized + 'static {
    /// Physical allocation representation used for this logical value.
    type Repr: Sized;

    /// Low tag stored in the raw Lisp [`Object`] word.
    const PRIMARY_TAG: u8;

    /// Allocation layout requested from MMTk.
    fn layout() -> Layout {
        Layout::new::<Self::Repr>()
    }

    /// Initialize a freshly allocated object representation.
    ///
    /// `start` is allocation start, not the object-reference pointer. Headered
    /// objects write [`Headered`] here. Headerless objects write their compact
    /// representation directly.
    ///
    /// # Safety
    ///
    /// `start` must point to writable memory of at least [`HeapObject::layout`]
    /// bytes and must not already contain an initialized value.
    unsafe fn init(start: Address, value: Self);

    /// Convert allocation start to the MMTk object-reference address.
    ///
    /// This is used immediately after allocation and while reconstructing raw
    /// tagged object words.
    ///
    /// # Safety
    ///
    /// `start` must be the allocation start for a valid object of this type.
    unsafe fn object_ref_from_start(start: Address) -> ObjectReference;

    /// Convert an MMTk object reference back to allocation start.
    ///
    /// MMTk copy and size callbacks need this address because they copy the
    /// entire allocation, not just the interior object-reference location.
    ///
    /// # Safety
    ///
    /// `object` must be an object reference for this heap object type.
    unsafe fn start_from_object_ref(object: ObjectReference) -> Address;

    /// Convert an MMTk object reference to a logical value pointer.
    ///
    /// Headered objects return the `Headered<T>::data` address. Headerless
    /// cons-like objects recover the allocation start from the interior object
    /// reference and return that as `*mut T`.
    ///
    /// # Safety
    ///
    /// `object` must be an object reference for this heap object type.
    unsafe fn data_from_object_ref(object: ObjectReference) -> NonNull<Self>;

    /// Convert a logical value pointer back to the MMTk object reference.
    ///
    /// This is used when a typed borrow or handle needs to be turned back into
    /// the erased representation expected by MMTk.
    ///
    /// # Safety
    ///
    /// `data` must point into a live object of this heap object type.
    unsafe fn object_ref_from_data(data: NonNull<Self>) -> ObjectReference;
}
