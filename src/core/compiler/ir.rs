use std::sync::Arc;
use crate::core::{ident::Ident, value::Vector};

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    Symbol(Ident),
    Vector(Vec<Expr>),
    Call(Call),
    SpecialForm(SpecialForm),
}

#[derive(Clone, Debug)]
pub enum SpecialForm {
    And(Vec<Expr>),
    Catch(Catch),
    Cond(Cond),
    ConditionCase(ConditionCase),
    Defconst(Defconst),
    Defvar(Defvar),
    Function(Function),
    If(If),
    Interactive(Interactive),
    Lambda(Lambda),
    Let(Let),
    LetStar(LetStar),
    Or(Vec<Expr>),
    Prog1(Prog1),
    Prog2(Prog2),
    Progn(Vec<Expr>),
    Quote(Quote),
    SaveCurrentBuffer(SaveCurrentBuffer),
    SaveExcursion(SaveExcursion),
    SaveRestriction(SaveRestriction),
    Set(Set),
    Setq(Setq),
    SetqDefault(SetqDefault),
    UnwindProtect(UnwindProtect),
    While(While),
}

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<(Ident, Option<Expr>)>,
    pub body: Vec<Expr>
}

#[derive(Debug, Clone)]
pub struct LetStar {
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
pub struct Setq {
    pub assignments: Vec<(Ident, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Defvar {
    pub symbol: Ident,
    pub value: Option<Box<Expr>>,
    pub docstring: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Defconst {
    pub symbol: Ident,
    pub value: Box<Expr>,
    pub docstring: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub clauses: Vec<CondClause>,
}

#[derive(Debug, Clone)]
pub struct CondClause {
    pub condition: Expr,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Box<Expr>,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Prog1 {
    pub first: Box<Expr>,
    pub rest: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Prog2 {
    pub first: Box<Expr>,
    pub second: Box<Expr>,
    pub rest: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Catch {
    pub tag: Box<Expr>,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnwindProtect {
    pub protected: Box<Expr>,
    pub cleanup: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ConditionCase {
    pub var: Option<Ident>,
    pub protected: Box<Expr>,
    pub handlers: Vec<ConditionHandler>,
}

#[derive(Debug, Clone)]
pub struct ConditionHandler {
    pub condition: Expr,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct SaveCurrentBuffer {
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct SaveExcursion {
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct SaveRestriction {
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct SetqDefault {
    pub assignments: Vec<(Ident, Expr)>,
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
