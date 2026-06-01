use thiserror::Error;

use crate::core::object::Object;

// Central tag table for manual tag management. Runtime code should still
// dispatch through each type's `Tagged::TAG`, not by matching these names.
/// Runtime tag returned by `Object::get_tag()` for the `t` constant.
pub const TAG_TRUE: u8 = 0xff;
/// Logical metadata tag for heap-allocated floats.
pub const TAG_FLOAT: u8 = 0b1010;
/// Logical metadata tag for strings.
pub const TAG_STR: u8 = 0b1011;
/// Logical metadata tag for vectors.
pub const TAG_VECTOR: u8 = 0b1100;
/// Logical metadata tag for functions and closures.
pub const TAG_FUNCTION: u8 = 0b1101;
/// Logical metadata tag for the global symbol table payload.
pub const TAG_SYMBOL_MAP: u8 = 0b1110;
/// Logical metadata tag for the dynamic binding stack payload.
pub const TAG_SPEC_STACK: u8 = 0b1111;
/// Logical metadata tag for compiler macro items.
pub const TAG_MACRO_ITEM: u8 = 0b1_0000;

#[derive(Error, Debug, Clone)]
pub enum TaggedPtrError {
    #[error("encountered null pointer")]
    NullPointer,
    #[error("type mismatch")]
    TypeMisMatch,
}

/// Convert a typed Lisp value into a raw tagged object word.
pub trait Tag {
    /// Encode `self` as an [`Object`].
    fn tag(self) -> Object;
}

/// Convert a raw tagged object word into a typed Lisp value.
pub trait Untag: Sized {
    /// Try to decode `object` as `Self`.
    fn untag(object: Object) -> std::result::Result<Self, TaggedPtrError>;
}

macro_rules! impl_tagged_for_gc {
    ($name:ident, $inner:ty) => {
        impl From<crate::gc::Gc<$inner>> for $name {
            fn from(value: crate::gc::Gc<$inner>) -> Self {
                Self(value)
            }
        }

        impl crate::core::tag::Tag for $name {
            fn tag(self) -> crate::core::object::Object {
                let raw = self.0.as_untagged_ptr().as_ptr() as usize as u64;
                crate::core::object::Object::from_raw(
                    raw | <$inner as crate::gc::HeapObject>::PRIMARY_TAG as u64,
                )
            }
        }

        impl crate::core::tag::Untag for $name {
            fn untag(
                object: crate::core::object::Object,
            ) -> std::result::Result<Self, crate::core::tag::TaggedPtrError> {
                if object.get_tag() != <$inner as crate::gc::Tagged>::TAG {
                    return Err(crate::core::tag::TaggedPtrError::TypeMisMatch);
                }

                let ptr = object
                    .untagged_ptr()
                    .ok_or(crate::core::tag::TaggedPtrError::NullPointer)?;
                Ok(Self(crate::gc::Gc::from_untagged_ptr(ptr)))
            }
        }
    };
}
