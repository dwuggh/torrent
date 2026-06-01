use std::ptr::NonNull;

use crate::{
    core::{
        cons::{Cons, LispCons},
        function::{Function, LispFunction},
        number::{Character, Float, Integer, LispCharacter, LispFloat, LispInteger},
        string::{LispStr, Str},
        symbol::{LispSymbol, Symbol, SymbolMap},
        tag::{TAG_TRUE, Tag, Untag},
        vector::{LispVector, Vector},
    },
    gc::{self, HeapObject, Tagged, Trace},
};

/// Raw tagged word for `nil`.
pub const NIL: i64 = gc::GcTag::NIL as i64;
/// Raw tagged word for `t`.
pub const TRUE: i64 = TAG_TRUE as i64;

#[repr(transparent)]
#[derive(PartialEq, PartialOrd, Eq, Hash)]
pub struct Object(pub u64);

impl Clone for Object {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl Default for Object {
    fn default() -> Self {
        nil()
    }
}

pub fn nil() -> Object {
    Object(NIL as u64)
}

pub fn tru() -> Object {
    Object(TRUE as u64)
}

impl Object {
    /// Create an object from a raw tagged word.
    #[inline(always)]
    pub fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    /// Return the raw tagged word.
    #[inline(always)]
    pub fn raw(&self) -> u64 {
        self.0
    }

    /// Return the low three-bit primary tag.
    #[inline(always)]
    pub fn primary_tag(&self) -> u8 {
        self.0 as u8 & gc::GcTag::MASK
    }

    /// Return the untagged object-reference pointer, if non-null.
    #[inline(always)]
    pub fn untagged_ptr(&self) -> Option<NonNull<()>> {
        let raw = self.0 & !(gc::GcTag::MASK as u64);
        NonNull::new(raw as *mut ())
    }

    /// Return the MMTk object reference encoded in this object word.
    ///
    /// # Safety
    ///
    /// The object must have a reference primary tag.
    #[inline(always)]
    pub unsafe fn object_ref_unchecked(&self) -> gc::mmtk::util::ObjectReference {
        let ptr = self
            .untagged_ptr()
            .expect("heap object pointer must not be null");
        unsafe {
            gc::mmtk::util::ObjectReference::from_raw_address_unchecked(
                gc::mmtk::util::Address::from_ptr(ptr.as_ptr()),
            )
        }
    }

    /// Return true if this word can hold an object reference.
    #[inline(always)]
    pub fn is_reference(&self) -> bool {
        matches!(
            self.primary_tag(),
            gc::GcTag::SECONDARY | gc::GcTag::CONS
        )
    }

    /// Return true for immediate, non-reference values.
    #[inline(always)]
    pub fn is_primitive(&self) -> bool {
        !self.is_reference()
    }

    /// Return the logical tag for this value.
    ///
    /// Headered objects read their logical tag from the heap header. Compact
    /// cons cells use their registered logical tag because they have no header.
    pub fn get_tag(&self) -> u8 {
        let primary_tag = self.primary_tag();

        match primary_tag {
            gc::GcTag::SECONDARY => unsafe {
                gc::logical_tag_for_object(self.object_ref_unchecked())
            },
            gc::GcTag::HEADER_MARKER_VALUE => unreachable!(),
            gc::GcTag::NIL => {
                // Nil-primary constants use their low byte as the full tag.
                self.0 as u8
            }
            _ => primary_tag,
        }
    }

    unsafe fn heap_ref<T: HeapObject>(&self) -> &T {
        let object = unsafe { self.object_ref_unchecked() };
        unsafe { T::data_from_object_ref(object).as_ref() }
    }

    unsafe fn heap_mut<T: HeapObject>(&mut self) -> &mut T {
        let object = unsafe { self.object_ref_unchecked() };
        unsafe { T::data_from_object_ref(object).as_mut() }
    }

    pub fn untag(self) -> LispObject {
        match self.get_tag() {
            gc::GcTag::NIL => LispObject::Nil,
            tag if tag == TRUE as u8 => LispObject::True,
            gc::GcTag::INT => LispObject::Int(LispInteger::untag(self).unwrap()),
            Float::TAG => {
                LispObject::Float(LispFloat::untag(self).unwrap())
            }
            gc::GcTag::CHAR => LispObject::Character(LispCharacter::untag(self).unwrap()),
            Str::TAG => {
                LispObject::Str(LispStr::untag(self).unwrap())
            }
            gc::GcTag::SYMBOL => LispObject::Symbol(LispSymbol::untag(self).unwrap()),
            Vector::TAG => {
                LispObject::Vector(LispVector::untag(self).unwrap())
            }
            gc::GcTag::CONS => LispObject::Cons(LispCons::untag(self).unwrap()),
            Function::TAG => {
                LispObject::Function(LispFunction::untag(self).unwrap())
            }
            _ => panic!("cannot untag internal object tag {}", self.get_tag()),
        }
    }

    pub fn as_ref(&self) -> ObjectRef<'_> {
        match self.get_tag() {
            gc::GcTag::NIL => ObjectRef::Nil,
            tag if tag == TRUE as u8 => ObjectRef::True,
            gc::GcTag::INT => {
                ObjectRef::Int(LispInteger::untag(Object::from_raw(self.0)).unwrap().0)
            }
            Float::TAG => {
                ObjectRef::Float(unsafe { self.heap_ref::<Float>() })
            }
            gc::GcTag::CHAR => {
                ObjectRef::Character(LispCharacter::untag(Object::from_raw(self.0)).unwrap().0)
            }
            Str::TAG => {
                ObjectRef::Str(unsafe { self.heap_ref::<Str>() })
            }
            gc::GcTag::SYMBOL => {
                ObjectRef::Symbol(LispSymbol::untag(Object::from_raw(self.0)).unwrap().0)
            }
            Vector::TAG => {
                ObjectRef::Vector(unsafe { self.heap_ref::<Vector>() })
            }
            gc::GcTag::CONS => ObjectRef::Cons(unsafe { self.heap_ref::<Cons>() }),
            Function::TAG => {
                ObjectRef::Function(unsafe { self.heap_ref::<Function>() })
            }
            _ => ObjectRef::Unknown,
        }
    }

    pub fn as_mut(&mut self) -> ObjectMut<'_> {
        match self.get_tag() {
            gc::GcTag::NIL => ObjectMut::Nil,
            tag if tag == TRUE as u8 => ObjectMut::True,
            gc::GcTag::INT => {
                ObjectMut::Int(LispInteger::untag(Object::from_raw(self.0)).unwrap().0)
            }
            Float::TAG => {
                ObjectMut::Float(unsafe { self.heap_mut::<Float>() })
            }
            gc::GcTag::CHAR => {
                ObjectMut::Character(LispCharacter::untag(Object::from_raw(self.0)).unwrap().0)
            }
            Str::TAG => {
                ObjectMut::Str(unsafe { self.heap_mut::<Str>() })
            }
            gc::GcTag::SYMBOL => {
                ObjectMut::Symbol(LispSymbol::untag(Object::from_raw(self.0)).unwrap().0)
            }
            Vector::TAG => {
                ObjectMut::Vector(unsafe { self.heap_mut::<Vector>() })
            }
            gc::GcTag::CONS => ObjectMut::Cons(unsafe { self.heap_mut::<Cons>() }),
            Function::TAG => {
                ObjectMut::Function(unsafe { self.heap_mut::<Function>() })
            }
            _ => ObjectMut::Unknown,
        }
    }
}

unsafe impl Trace for Object {
    unsafe fn trace(&self, visitor: &mut gc::Visitor) {
        if self.is_reference() {
            // `self` is a field-sized tagged slot. MMTk may rewrite the word
            // during tracing if the referenced object moves.
            let slot = gc::mmtk::util::Address::from_ptr(self as *const Object as *mut Object);
            visitor.visit_slot_address(slot);
        }
    }
}

#[derive(Clone, Debug, Default)]
pub enum LispObject {
    #[default]
    Nil,
    True,
    Int(LispInteger),
    Float(LispFloat),
    Character(LispCharacter),
    Str(LispStr),
    Symbol(LispSymbol),
    Vector(LispVector),
    Cons(LispCons),
    Function(LispFunction),
}

#[derive(Debug, Clone, Copy, Default)]
pub enum ObjectRef<'a> {
    #[default]
    Nil,
    True,
    Int(Integer),
    Float(&'a Float),
    Character(Character),
    Symbol(Symbol),
    Str(&'a Str),
    Vector(&'a Vector),
    Cons(&'a Cons),
    Function(&'a Function),
    Unknown,
}

#[derive(Debug, Default)]
pub enum ObjectMut<'a> {
    #[default]
    Nil,
    True,
    Int(Integer),
    Float(&'a mut Float),
    Character(Character),
    Symbol(Symbol),
    Str(&'a mut Str),
    Vector(&'a mut Vector),
    Cons(&'a mut Cons),
    Function(&'a mut Function),
    Unknown,
}

impl LispObject {
    pub fn tag(self) -> Object {
        match self {
            LispObject::Nil => nil(),
            LispObject::True => tru(),
            LispObject::Int(integer) => integer.tag(),
            LispObject::Float(float) => float.tag(),
            LispObject::Character(character) => character.tag(),
            LispObject::Str(lisp_string) => lisp_string.tag(),
            LispObject::Symbol(symbol) => symbol.tag(),
            LispObject::Vector(vector) => vector.tag(),
            LispObject::Cons(cons) => cons.tag(),
            LispObject::Function(function) => function.tag(),
        }
    }
}

/// Shared helper for storing a tagged object into a heap field.
///
/// The owner computes its object reference before selecting the field slot.
/// The selector is monomorphized and should optimize to direct field-address
/// code, while keeping the MMTk pre-barrier sequence in one place.
pub trait HeapSlotUpdate: HeapObject {
    fn update_slot<F>(&mut self, select: F, value: Object)
    where
        F: FnOnce(&mut Self) -> &mut Object,
    {
        let src = unsafe { Self::object_ref_from_data(NonNull::from(&mut *self)) };
        let slot = select(self);
        write_object_slot(src, slot, value);
    }
}

impl<T: HeapObject> HeapSlotUpdate for T {}

/// Store `value` into a tagged object slot after running MMTk's pre-barrier.
pub fn write_object_slot(
    src: gc::mmtk::util::ObjectReference,
    slot: &mut Object,
    value: Object,
) {
    let slot_addr = gc::mmtk::util::Address::from_ptr(slot as *mut Object);
    gc::object_slot_write_pre(src, slot_addr);
    *slot = value;
}

macro_rules! impl_try_from_for_object {
    ($name:ident, $variant:ident) => {
        impl<'a> TryFrom<&'a Object> for &'a $name {
            type Error = &'static str;

            fn try_from(object: &'a Object) -> Result<Self, Self::Error> {
                match object.as_ref() {
                    ObjectRef::$variant(value) => Ok(value),
                    _ => Err("wrong type"),
                }
            }
        }

        impl<'a> TryFrom<&'a Object> for &'a mut $name {
            type Error = &'static str;

            fn try_from(object: &'a Object) -> Result<Self, Self::Error> {
                if object.get_tag() != <$name as crate::gc::Tagged>::TAG {
                    return Err("wrong type");
                }

                let object_ref = unsafe { object.object_ref_unchecked() };
                let mut data = unsafe { <$name as HeapObject>::data_from_object_ref(object_ref) };
                Ok(unsafe { data.as_mut() })
            }
        }
    };
}

macro_rules! impl_try_from_for_immediate {
    ($name:ident, $variant:ident) => {
        impl TryFrom<&Object> for $name {
            type Error = &'static str;

            fn try_from(object: &Object) -> Result<Self, Self::Error> {
                match object.as_ref() {
                    ObjectRef::$variant(value) => Ok(value),
                    _ => Err("wrong type"),
                }
            }
        }
    };
}

impl_try_from_for_immediate!(Integer, Int);
impl_try_from_for_immediate!(Character, Character);
impl_try_from_for_immediate!(Symbol, Symbol);

impl<'a> TryFrom<&'a Object> for &'a Float {
    type Error = &'static str;

    fn try_from(object: &'a Object) -> Result<Self, Self::Error> {
        match object.as_ref() {
            ObjectRef::Float(value) => Ok(value),
            _ => Err("wrong type"),
        }
    }
}

impl_try_from_for_object!(Str, Str);
impl_try_from_for_object!(Vector, Vector);
impl_try_from_for_object!(Cons, Cons);
impl_try_from_for_object!(Function, Function);

/// Register all heap object metadata used by the runtime.
pub fn init_gc_types() {
    gc::register_headerless_metadata_tag(gc::GcTag::CONS);
    gc::register_heap_object::<Float>();
    gc::register_heap_object::<Str>();
    gc::register_heap_object::<Vector>();
    gc::register_heap_object::<Cons>();
    gc::register_heap_object::<Function>();
    gc::register_heap_object::<SymbolMap>();
    gc::register_heap_object::<crate::core::env::SpecStack>();
    gc::register_heap_object::<crate::core::compiler::macro_item::MacroItemType>();
}

#[cfg(test)]
mod tests {
    use crate::core::{
        cons::{Cons, LispCons},
        number::{Float, LispFloat, LispInteger},
        Tag,
    };

    use super::{ObjectRef, gc};

    /// Keeps the current thread bound to MMTk for the duration of an allocation test.
    struct MutatorGuard;

    impl Drop for MutatorGuard {
        fn drop(&mut self) {
            gc::destroy_mutator();
        }
    }

    fn bind_nogc_mutator() -> MutatorGuard {
        let mmtk = *gc::MMTK.get_or_init(|| {
            let mut builder = gc::mmtk::MMTKBuilder::new_no_env_vars();
            assert!(builder.set_option("plan", "NoGC"));
            assert!(builder.set_option("threads", "1"));

            // The root crate owns the global MMTk instance; this test chooses
            // NoGC as the first concrete plan used by that instance.
            let mmtk = Box::leak(gc::mmtk::memory_manager::mmtk_init::<gc::MM>(&builder));
            gc::initialize_collection(mmtk);
            mmtk
        });

        gc::bind_mutator(mmtk);
        MutatorGuard
    }

    #[test]
    fn nogc_allocates_integer_float_and_cons() {
        let _mutator = bind_nogc_mutator();

        gc::register_headerless_metadata_tag(gc::GcTag::CONS);
        gc::register_heap_object::<Float>();
        gc::register_heap_object::<Cons>();

        let integer = LispInteger(42).tag();
        assert_eq!(integer.get_tag(), gc::GcTag::INT);
        assert!(matches!(integer.as_ref(), ObjectRef::Int(42)));

        let float = LispFloat::new(3.5).tag();
        assert_eq!(float.get_tag(), <Float as gc::Tagged>::TAG);
        match float.as_ref() {
            ObjectRef::Float(value) => assert_eq!(value.0, 3.5),
            other => panic!("expected float object, got {other:?}"),
        }

        let cons = LispCons::new(integer.clone(), float.clone()).tag();
        assert_eq!(cons.get_tag(), gc::GcTag::CONS);
        let ObjectRef::Cons(pair) = cons.as_ref() else {
            panic!("expected cons object, got {:?}", cons.as_ref());
        };

        assert!(matches!(pair.car().as_ref(), ObjectRef::Int(42)));
        match pair.cdr().as_ref() {
            ObjectRef::Float(value) => assert_eq!(value.0, 3.5),
            other => panic!("expected cdr float object, got {other:?}"),
        }
    }
}
