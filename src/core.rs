#[macro_use]
pub mod tagged_ptr;

pub mod compiler;
pub mod cons;
pub mod env;
pub mod function;
pub mod hashtable;
pub mod ident;
pub mod indirect;
pub mod number;
pub mod object;
pub mod parser;
pub mod runtime;
pub mod string;
pub mod symbol;
pub mod vector;

pub use tagged_ptr::Tagged;

#[macro_use]
pub mod error;
