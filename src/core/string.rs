use std::sync::Arc;

use crate::core::value::Value;


pub type LispString = Arc<String>;

impl TryFrom<Value> for LispString {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.untag()
    }
}