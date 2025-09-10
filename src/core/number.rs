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