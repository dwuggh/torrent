use std::convert::Infallible;

use crate::core::{value::LispType, TaggedPtr};


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer(i64);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Float(f64);


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Character(char);

impl TaggedPtr for Integer {
    const TAG: LispType = LispType::Int;

    type Data = i64;

    type Inner = Integer;

    unsafe fn to_raw(&self) -> u64 {
        self.0 as u64
    }

}

impl AsRef<i64> for Integer {
    fn as_ref(&self) -> &i64 {
        &self.0
    }
}

impl AsMut<i64> for Integer {
    fn as_mut(&mut self) -> &mut i64 {
        &mut self.0
    }
}

impl TryFrom<*mut Integer> for Integer {
    type Error = Infallible;

    fn try_from(value: *mut Integer) -> Result<Self, Self::Error> {
        Ok(Self(value as i64))
    }
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Self(value)
    }
    
    pub fn value(&self) -> i64 {
        self.0
    }
}

impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl From<Integer> for i64 {
    fn from(value: Integer) -> Self {
        value.0
    }
}

impl TaggedPtr for Float {
    const TAG: LispType = LispType::Float;
    type Data = f64;
    type Inner = Float;

    unsafe fn to_raw(&self) -> u64 {
        self.0.to_bits()
    }
}

impl AsRef<f64> for Float {
    fn as_ref(&self) -> &f64 {
        &self.0
    }
}

impl AsMut<f64> for Float {
    fn as_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}

impl TryFrom<*mut Float> for Float {
    type Error = Infallible;

    fn try_from(value: *mut Float) -> Result<Self, Self::Error> {
        Ok(Self(f64::from_bits(value as u64)))
    }
}

impl Float {
    pub fn new(value: f64) -> Self {
        Self(value)
    }
    
    pub fn value(&self) -> f64 {
        self.0
    }
}

impl From<f64> for Float {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl From<Float> for f64 {
    fn from(value: Float) -> Self {
        value.0
    }
}

impl TaggedPtr for Character {
    const TAG: LispType = LispType::Character;
    type Data = char;
    type Inner = Character;

    unsafe fn to_raw(&self) -> u64 {
        self.0 as u32 as u64
    }
}

impl AsRef<char> for Character {
    fn as_ref(&self) -> &char {
        &self.0
    }
}

impl AsMut<char> for Character {
    fn as_mut(&mut self) -> &mut char {
        &mut self.0
    }
}

impl TryFrom<*mut Character> for Character {
    type Error = Infallible;

    fn try_from(value: *mut Character) -> Result<Self, Self::Error> {
        let char_val = char::from_u32(value as u32).unwrap_or('\0');
        Ok(Self(char_val))
    }
}

impl Character {
    pub fn new(value: char) -> Self {
        Self(value)
    }
    
    pub fn value(&self) -> char {
        self.0
    }
}

impl From<char> for Character {
    fn from(value: char) -> Self {
        Self(value)
    }
}

impl From<Character> for char {
    fn from(value: Character) -> Self {
        value.0
    }
}
