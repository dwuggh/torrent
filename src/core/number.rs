use std::convert::Infallible;

use crate::core::{value::LispType, TaggedPtr};


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Integer(i64);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Float(f64);


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Character(char);

impl TaggedPtr for Integer {
    const TAG = LispType::Int;

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
    const TAG = LispType::Float;
    type Data = f64;
    type Inner = Float;

    unsafe fn to_raw(&self) -> u64 {
        self.0.to_bits()
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
    const TAG = LispType::Character;
    type Data = char;
    type Inner = Character;

    unsafe fn to_raw(&self) -> u64 {
        self.0 as u32 as u64
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
