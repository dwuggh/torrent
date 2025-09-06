// use crate::core::symbol::Symbol;

use std::sync::Arc;

use crate::core::{ident::Ident, value::Vector};

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    Symbol(Ident),
    Vector(Vec<Expr>),
    Let(Let),
    If(If),
    Lambda(Lambda),
    Quote(Quote),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Set(Set),
    Fset(Fset),
    Call(Call),
    Progn(Vec<Expr>),
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
    pub args: Vec<Arg>,
    // NOTE if there's exactly 1 string after args and no other exprs,
    // then this string should be placed in body.
    // i.e. (lambda () "foo") should return "foo"
    pub docstring: Option<String>,
    pub interactive: Option<Interactive>,
    pub declare: Option<Vec<Expr>>,
    pub body: Vec<Expr>,
    pub captures: Vec<Ident>, // For closure analysis
}

#[derive(Debug, Clone)]
pub struct Interactive {
    pub arg_desc: Option<String>,
    pub modes: Vec<Ident>
}

#[derive(Debug, Clone)]
pub struct Call {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub symbol: Ident,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Fset {
    pub symbol: Ident,
    pub function: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub struct Arg {
    pub ty: ArgsType,
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ArgsType {
    Normal,
    Optional,
    Rest,
}

#[derive(Debug, Clone)]
pub struct Quote {
    pub kind: QuoteKind,
    pub expr: QuotedData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuoteKind {
    Quote,      // 'expr
    Backquote,  // `expr
}

#[derive(Debug, Clone)]
pub enum QuotedData {
    // Simple literals that evaluate to themselves
    Literal(Literal),
    Symbol(Ident),

    // Collections that may contain unquotes
    List(Vec<QuotedData>),
    Vector(Vec<QuotedData>),

    // Unquoting (only valid inside backquotes)
    Unquote(Box<Expr>),        // ,expr
    UnquoteSplice(Box<Expr>),  // ,@expr
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
