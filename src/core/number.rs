
use crate::core::{
    object::LispType,
    tagged_ptr::{shifting_tag, shifting_untag},
    Tagged,
};

// pub(crate) const MAX_FIXNUM: i64 = i64::MAX >> 8;
// pub(crate) const MIN_FIXNUM: i64 = i64::MIN >> 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LispInteger(pub i64);
pub type Integer = i64;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct LispFloat(pub f64);
pub type Float = f64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LispCharacter(pub Character);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Character(pub u64);

impl Character {
    pub fn new(c: char) -> Self {
        Self(c as u64)
    }
    /// Tries to convert the `Char64` back into a standard `char`.
    /// Returns `None` if the value is out of range or not a valid character.
    pub fn to_char(&self) -> Option<char> {
        // First, try to convert the u64 to a u32.
        // Then, try to convert the u32 into a valid char.
        u32::try_from(self.0).ok().and_then(std::char::from_u32)
    }
}

impl From<char> for Character {
    fn from(c: char) -> Self {
        Self(c as u64)
    }
}

impl std::fmt::Display for Character {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use the `to_char` method we just created.
        // If it returns `None`, fall back to the replacement character.
        let c = self.to_char().unwrap_or(' ');
        write!(f, "{}", c)
    }
}

impl_tagged_for_prim!(LispInteger, LispType::Int, Integer);
impl_tagged_for_prim!(LispFloat, LispType::Float, Float);
impl Tagged for LispCharacter {
    const TAG: LispType = (LispType::Character);
    type Data<'a> = Character;
    type DataMut<'a> = Character;
    unsafe fn to_raw(&self) -> u64 {
        shifting_tag(self.0 .0, Self::TAG)
    }
    unsafe fn from_raw(raw: u64) -> Self {
        std::mem::transmute(shifting_untag(raw))
    }
    unsafe fn cast<'a>(val: u64) -> Self::Data<'a> {
        Self::from_raw(val).0
    }
    unsafe fn cast_mut<'a>(val: u64) -> Self::DataMut<'a> {
        Self::from_raw(val).0
    }
}

impl AsRef<i64> for LispInteger {
    fn as_ref(&self) -> &i64 {
        &self.0
    }
}

impl AsMut<i64> for LispInteger {
    fn as_mut(&mut self) -> &mut i64 {
        &mut self.0
    }
}

impl LispInteger {
    pub fn new(value: i64) -> Self {
        Self(value)
    }

    pub fn value(&self) -> i64 {
        self.0
    }
}

impl From<i64> for LispInteger {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl From<LispInteger> for i64 {
    fn from(value: LispInteger) -> Self {
        value.0
    }
}

impl AsRef<f64> for LispFloat {
    fn as_ref(&self) -> &f64 {
        &self.0
    }
}

impl AsMut<f64> for LispFloat {
    fn as_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}

impl LispFloat {
    pub fn new(value: f64) -> Self {
        Self(value)
    }

    pub fn value(&self) -> f64 {
        self.0
    }
}

impl From<f64> for LispFloat {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl From<LispFloat> for f64 {
    fn from(value: LispFloat) -> Self {
        value.0
    }
}

impl AsRef<Character> for LispCharacter {
    fn as_ref(&self) -> &Character {
        &self.0
    }
}

impl AsMut<Character> for LispCharacter {
    fn as_mut(&mut self) -> &mut Character {
        &mut self.0
    }
}

impl LispCharacter {
    pub fn new(value: char) -> Self {
        Self(Character::new(value))
    }

    pub fn value(&self) -> char {
        self.0.to_char().unwrap()
    }
}

impl From<char> for LispCharacter {
    fn from(value: char) -> Self {
        Self::new(value)
    }
}

impl From<LispCharacter> for char {
    fn from(value: LispCharacter) -> Self {
        value.value()
    }
}
