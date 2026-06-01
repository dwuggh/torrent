use std::{
    alloc::Layout,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ptr::NonNull,
    sync::{
        atomic::{AtomicU8, Ordering},
        LazyLock, RwLock,
    },
};

use mmtk::{
    util::{Address, ObjectReference},
    AllocationSemantics,
};

use super::{post_alloc_with_semantics, thread::current_thread, Tagged, OBJECT_REF_OFFSET};

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
    /// Mask for the low primary tag bits.
    pub const MASK: u8 = 0b111;
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
    unsafe fn finalize(&mut self) {
        unsafe { std::ptr::drop_in_place(self) };
    }
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
static HEADERLESS_METADATA_TAG: AtomicU8 = AtomicU8::new(GcTag::CONS);

/// Register erased metadata for `tag`.
///
/// This is intentionally separate from [`Tagged`] so the GC crate can stay
/// independent of the runtime crate that owns the actual Lisp object types.
pub fn register_metadata(tag: u8, metadata: MetaData) {
    let mut table = VTABLE.write().expect("metadata table lock poisoned");
    table[tag as usize] = Some(metadata);
}

/// Register the logical tag used when an object has no header word.
///
/// Torrent currently uses this for compact cons cells. Headerless metadata
/// lookup cannot read a [`GcHeader`], so the runtime must tell the GC binding
/// which logical tag to use for such objects.
pub fn register_headerless_metadata_tag(tag: u8) {
    HEADERLESS_METADATA_TAG.store(tag, Ordering::Relaxed);
}

/// Build an erased metadata entry for a concrete heap object type.
pub fn metadata_for<T: HeapObject>() -> MetaData {
    MetaData {
        layout: T::layout(),
        trace: trace_erased::<T>,
        finalize: finalize_erased::<T>,
        size: size_erased::<T>,
    }
}

/// Register metadata for a concrete heap object type.
pub fn register_heap_object<T: HeapObject>() {
    register_metadata(T::TAG, metadata_for::<T>());
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
    let tag = unsafe { logical_tag_for_object(object) };
    get_metadata_for_tag(tag).expect("no metadata for object tag")
}

/// Return the logical metadata tag for an erased object reference.
///
/// Headered objects carry the tag in [`GcHeader`]. Headerless objects use the
/// runtime-registered headerless logical tag.
///
/// # Safety
///
/// `object` must be an object reference produced by this VM binding.
pub unsafe fn logical_tag_for_object(object: ObjectReference) -> u8 {
    let start = object.to_raw_address().sub(OBJECT_REF_OFFSET);
    let first_word: u64 = unsafe { start.load() };

    if GcHeader::is_header_word(first_word) {
        let header = unsafe { start.as_ref::<GcHeader>() };
        header.tag()
    } else {
        HEADERLESS_METADATA_TAG.load(Ordering::Relaxed)
    }
}

/// Header word stored at the allocation start of secondary objects.
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct GcHeader {
    /// Encoded header payload.
    ///
    /// The low three bits are the header marker. The next byte stores the full
    /// logical object tag used for metadata lookup. MMTk forwarding bits live
    /// in side metadata so they cannot corrupt Torrent's tag bits while an
    /// object is being forwarded.
    data: u64,
}

impl GcHeader {
    /// Create a header for a logical object tag.
    pub fn new(tag: u8) -> Self {
        Self {
            data: (tag as u64) << 3 | GcTag::HEADER_MARKER as u64,
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
        ((self.data >> 3) & 0xff) as u8
    }

    /// Return erased metadata for this header's logical tag.
    pub fn metadata(&self) -> MetaData {
        get_metadata_for_tag(self.tag()).expect("no metadata for tag")
    }
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
        let size = T::layout().size();
        let start = thread.alloc(size);
        unsafe { T::init(start, value) };
        let object = unsafe { T::object_ref_from_start(start) };
        post_alloc_with_semantics(thread.mutator(), object, size, AllocationSemantics::Default);
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

    /// Return this handle as an MMTk object reference.
    pub fn object_ref(&self) -> ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(Address::from_ptr(self.ptr.as_ptr())) }
    }

    /// Create a typed handle from an untagged object-reference pointer.
    ///
    /// The main crate owns Lisp `Object` tagging. It clears the low primary
    /// bits and delegates the untagged pointer to this constructor.
    pub fn from_untagged_ptr(ptr: NonNull<()>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Return the untagged pointer stored by this handle.
    pub fn as_untagged_ptr(&self) -> NonNull<()> {
        self.ptr
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

impl<T: HeapObject> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl<T: HeapObject + Debug> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T: HeapObject + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: HeapObject + Eq> Eq for Gc<T> {}

impl<T: HeapObject + PartialOrd> PartialOrd for Gc<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<T> Default for Gc<T>
where
    T: HeapObject + Default,
{
    fn default() -> Self {
        Self::new(T::default())
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

/// Marker for heap objects that use the default one-word header layout.
///
/// This exists for Rust coherence: the blanket [`HeapObject`] implementation
/// applies only to types that opt into headered layout, leaving room for
/// compact custom layouts such as cons cells.
///
/// # Safety
///
/// Implementors must be valid as the `data` field of [`Headered<Self>`].
pub unsafe trait HeaderedObject: Tagged + Trace + Sized + 'static {}

unsafe impl<T> HeapObject for T
where
    T: HeaderedObject,
{
    type Repr = Headered<T>;

    const PRIMARY_TAG: u8 = GcTag::SECONDARY;

    unsafe fn init(start: Address, value: Self) {
        let repr = Headered {
            header: GcHeader::new(Self::TAG),
            data: value,
        };
        unsafe { start.store::<Headered<T>>(repr) };
    }
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
    #[inline(always)]
    unsafe fn object_ref_from_start(start: Address) -> ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(start + OBJECT_REF_OFFSET) }
    }

    /// Convert an MMTk object reference back to allocation start.
    ///
    /// MMTk copy and size callbacks need this address because they copy the
    /// entire allocation, not just the interior object-reference location.
    ///
    /// # Safety
    ///
    /// `object` must be an object reference for this heap object type.
    #[inline(always)]
    unsafe fn start_from_object_ref(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_REF_OFFSET)
    }

    /// Convert an MMTk object reference to a logical value pointer.
    ///
    /// Headered objects return the `Headered<T>::data` address. Headerless
    /// cons-like objects recover the allocation start from the interior object
    /// reference and return that as `*mut T`.
    ///
    /// # Safety
    ///
    /// `object` must be an object reference for this heap object type.
    #[inline(always)]
    unsafe fn data_from_object_ref(object: ObjectReference) -> NonNull<Self> {
        unsafe { NonNull::new_unchecked(object.to_raw_address().to_mut_ptr::<Self>()) }
    }

    /// Convert a logical value pointer back to the MMTk object reference.
    ///
    /// This is used when a typed borrow or handle needs to be turned back into
    /// the erased representation expected by MMTk.
    ///
    /// # Safety
    ///
    /// `data` must point into a live object of this heap object type.
    #[inline(always)]
    unsafe fn object_ref_from_data(data: NonNull<Self>) -> ObjectReference {
        unsafe { ObjectReference::from_raw_address_unchecked(Address::from_ptr(data.as_ptr())) }
    }
}

unsafe fn trace_erased<T: HeapObject>(object: ObjectReference, visitor: &mut Visitor) {
    let data = unsafe { T::data_from_object_ref(object) };
    unsafe { data.as_ref().trace(visitor) };
}

unsafe fn finalize_erased<T: HeapObject>(object: ObjectReference) {
    let mut data = unsafe { T::data_from_object_ref(object) };
    unsafe { data.as_mut().finalize() };
}

unsafe fn size_erased<T: HeapObject>(_object: ObjectReference) -> usize {
    T::layout().size()
}
