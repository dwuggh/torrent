// use crate::core::symbol::Symbol;

use std::sync::Arc;

use crate::core::{ident::Ident, value::Vector};

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    Let(Let),
    If(If),
    Lambda(Lambda),
    Apply(),
    Quote(Quote),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Set(),
    Fset(),
    Vector(Vector),
    Sexp(Vec<Expr>)
}

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<(Ident, Option<Expr>)>,
    pub body: Vec<Expr>
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Arc<Expr>,
    pub then: Arc<Expr>,
    pub els: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub Args: Vec<Arg>,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum ArgsType {
    Normal,
    Optional,
    Rest,
}

#[derive(Debug, Clone, Copy)]
pub struct Arg {
    ty: ArgsType,
    ident: Ident
}

#[derive(Debug, Clone)]
pub struct Quote {
}


#[derive(Clone, Debug, PartialEq)]
pub enum Number {
    FixedInteger(i64),
    Real(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(Number),
    Boolean(bool),
    Character(char),
    String(String),
}


// #[derive(Debug, Clone, PartialEq, PartialOrd)]
// pub enum Node {
//     Ident(Symbol),
//     Sexp(Vec<Node>),
//     Vector(Vec<Node>),
//     // Value(Value),
//     Integer(i64),
//     Float(f64),
//     Char(char),
//     Str(String),
//     Unquote,
//     UnquoteSplice,
//     Backquote,
//     Nil,
// }

// pub struct Var {
//     symbol: Symbol,
// }
