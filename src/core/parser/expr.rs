use crate::core::{
    ident::Ident,
    number::{LispCharacter, LispFloat, LispInteger},
    parser::Span,
    string::LispStr,
    symbol::Symbol,
};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub struct Expr {
    pub ty: RefCell<ExprType>,
    pub span: Span,
}

impl Expr {
    pub fn new(ty: ExprType, span: Span) -> Self {
        let ty = RefCell::new(ty);
        Self { ty, span }
    }

    pub fn ty(&self) -> std::cell::Ref<'_, ExprType> {
        self.ty.borrow()
    }
}

#[derive(Clone, Debug)]
pub enum ExprType {
    Nil,
    Literal(Literal),
    Symbol(Cell<Var>),
    Vector(Vec<Expr>),
    Call(Call),
    SpecialForm(SpecialForm),
}

pub type RefExpr = Expr;
pub type BoxRefExpr = Box<Expr>;
pub type CellVar = Cell<Var>;

#[derive(Debug, Clone)]
pub struct Progn {
    pub body: Vec<Expr>,
}

impl Progn {
    pub fn new(body: Vec<Expr>) -> Self {
        Self { body }
    }
}

pub fn new_refbox(expr: Expr) -> BoxRefExpr {
    Box::new(expr)
}

#[derive(Clone, Debug, Copy)]
pub enum Var {
    Symbol(Symbol),
    Local(Local),
}

impl From<Var> for Ident {
    fn from(var: Var) -> Self {
        match var {
            Var::Symbol(symbol) => symbol.into(),
            Var::Local(local) => local.ident,
        }
    }
}

impl From<Var> for Symbol {
    fn from(var: Var) -> Self {
        match var {
            Var::Symbol(symbol) => symbol,
            Var::Local(local) => local.ident.into(),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Local {
    pub ident: Ident,
    pub id: u32,
}

#[derive(Clone, Debug)]
pub enum SpecialForm {
    And(Vec<RefExpr>),
    Or(Vec<RefExpr>),
    If(If),
    Catch(Catch),
    Cond(Cond),
    ConditionCase(ConditionCase),
    Defconst(Defconst),
    Defvar(Defvar),
    Function(Function),
    Interactive(Interactive),
    Lambda(Lambda),
    Let(Let),
    LetStar(LetStar),
    Prog1(Prog1),
    Prog2(Prog2),
    Progn(Progn),
    Quote(Quote),
    SaveCurrentBuffer(Progn),
    SaveExcursion(Progn),
    SaveRestriction(Progn),
    Set(Set),
    Setq(Setq),
    SetqDefault(SetqDefault),
    UnwindProtect(UnwindProtect),
    While(While),
}

// enum PrognType {
//     Normal,
//     SaveExcursion,
//     SaveRestriction,
// }

type Binding = Vec<(Ident, Option<Expr>)>;

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Rc<Binding>,
    pub body: Progn,
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct LetStar {
    pub bindings: Rc<Binding>,
    pub body: Progn,
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: BoxRefExpr,
    pub then: BoxRefExpr,
    pub els: Progn,
}

#[derive(Debug, Clone)]
pub struct Args {
    pub normal: Vec<Ident>,
    pub optional: Option<Vec<Ident>>,
    pub rest: Option<Ident>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub args: Rc<Args>,
    // NOTE if there's exactly 1 string after args and no other exprs,
    // then this string should be placed in body.
    // i.e. (lambda () "foo") should return "foo"
    pub docstring: Option<String>,
    pub interactive: Option<Interactive>,
    pub declare: Option<Vec<Expr>>,
    pub body: Progn,
    pub captures: RefCell<Vec<Var>>, // For closure analysis
}

#[derive(Debug, Clone)]
pub struct Interactive {
    pub arg_desc: Option<String>,
    pub modes: Option<Vec<CellVar>>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub symbol: CellVar,
    pub args: Vec<RefExpr>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub symbol: Ident,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Setq {
    pub assignments: Vec<(Ident, RefExpr)>,
}

#[derive(Debug, Clone)]
pub struct Defvar {
    pub symbol: Ident,
    pub value: Option<BoxRefExpr>,
    pub docstring: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Defconst {
    pub symbol: Ident,
    pub value: BoxRefExpr,
    pub docstring: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub clauses: Vec<CondClause>,
}

#[derive(Debug, Clone)]
pub struct CondClause {
    pub condition: RefExpr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: BoxRefExpr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct Prog1 {
    pub first: BoxRefExpr,
    pub rest: Progn,
}

#[derive(Debug, Clone)]
pub struct Prog2 {
    pub first: BoxRefExpr,
    pub second: BoxRefExpr,
    pub rest: Progn,
}

#[derive(Debug, Clone)]
pub struct Catch {
    pub tag: BoxRefExpr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct UnwindProtect {
    pub protected: BoxRefExpr,
    pub cleanup: Progn,
}

#[derive(Debug, Clone)]
pub struct ConditionCase {
    pub var: Option<Ident>,
    pub protected: BoxRefExpr,
    pub handlers: Vec<ConditionHandler>,
}

#[derive(Debug, Clone)]
pub struct ConditionHandler {
    pub condition: RefExpr,
    pub body: Progn,
}

// #[derive(Debug, Clone)]
// pub struct SaveCurrentBuffer {
//     pub body: Vec<Expr>,
// }

// #[derive(Debug, Clone)]
// pub struct SaveExcursion {
//     pub body: Vec<Expr>,
// }

// #[derive(Debug, Clone)]
// pub struct SaveRestriction {
//     pub body: Vec<Expr>,
// }

#[derive(Debug, Clone)]
pub struct SetqDefault {
    pub assignments: Vec<(Ident, RefExpr)>,
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
    Quote,     // 'expr
    Backquote, // `expr
}

#[derive(Debug, Clone)]
pub enum QuotedData {
    // Simple literals that evaluate to themselves
    Literal(Literal),
    Symbol(CellVar),

    // Collections that may contain unquotes
    List(Vec<QuotedData>),
    Vector(Vec<QuotedData>),

    // Unquoting (only valid inside backquotes)
    Unquote(BoxRefExpr),       // ,expr
    UnquoteSplice(BoxRefExpr), // ,@expr
}

#[derive(Clone, Debug, PartialEq)]
pub enum Number {
    Integer(LispInteger),
    Real(LispFloat),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(Number),
    Character(LispCharacter),
    String(LispStr),
}

impl ExprType {
    pub fn new_int(n: i64) -> Self {
        Self::Literal(Literal::Number(Number::Integer(LispInteger::new(n))))
    }

    pub fn new_float(f: f64) -> Self {
        Self::Literal(Literal::Number(Number::Real(LispFloat::new(f))))
    }

    pub fn new_char(c: char) -> Self {
        Self::Literal(Literal::Character(LispCharacter::new(c)))
    }

    pub fn new_str(str: String) -> Self {
        Self::Literal(Literal::String(LispStr::new(str)))
    }
}

// impl TryFrom<LispValue> for Expr {
//     type Error = RuntimeError;

//     fn try_from(value: LispValue) -> Result<Self, Self::Error> {
//         let node: Node = value.try_into()?;

//         let expr = node_to_ir(node).map_err(|e| RuntimeError::MacroExpansionError {
//             message: e.to_string(),
//         })?;
//         Ok(expr)
//     }
// }

// impl TryFrom<LispValue> for Node {
//     type Error = RuntimeError;

//     fn try_from(value: LispValue) -> Result<Self, Self::Error> {
//         let result = match value {
//             LispValue::Nil => Self::Nil,
//             LispValue::True => Self::Ident(Ident::from_string("t")),
//             LispValue::Int(i) => Self::Integer(i),
//             LispValue::Float(f) => Self::Float(f),
//             LispValue::Character(c) => Self::Char(c),
//             LispValue::String(lisp_string) => Self::Str(lisp_string.to_string()),
//             LispValue::Symbol(symbol) => Self::Ident(symbol.name),
//             LispValue::MacroItem(macro_item) => macro_item.try_into()?,
//             _ => {
//                 let err = Err(RuntimeError::MacroExpansionError {
//                     message: format!("failed at {value:?}"),
//                 });
//                 return err;
//             }
//         };
//         Ok(result)
//     }
// }

// impl TryFrom<MacroItem> for Node {
//     type Error = RuntimeError;
//     fn try_from(value: MacroItem) -> Result<Self, Self::Error> {
//         match value {
//             MacroItem::List(lisp_values) => {
//                 let mut sexp = Vec::new();
//                 for val in lisp_values.into_iter() {
//                     let node: Node = val.try_into()?;
//                     sexp.push(node);
//                 }
//                 Ok(Node::Sexp(sexp))
//             }
//             _ => todo!(),
//         }
//     }
// }
