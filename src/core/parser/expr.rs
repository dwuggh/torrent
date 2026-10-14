use crate::core::{
    cons::LispCons,
    ident::Ident,
    number::{LispCharacter, LispFloat, LispInteger},
    object::{LispObject, nil},
    parser::Span,
    string::LispStr,
    symbol::{LispSymbol, Symbol},
    tag::Tag,
    vector::LispVector,
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

#[derive(Clone, Debug)]
pub enum ExprType {
    Nil,
    Literal(Literal),
    Symbol(Cell<Var>),
    Vector(Vec<Expr>),
    Call(Call),
    SpecialForm(SpecialForm),
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

pub type BoxExpr = Box<Expr>;
pub type CellVar = Cell<Var>;

pub fn new_cellvar(ident: Ident) -> CellVar {
    Cell::new(Var::Unresolved(ident))
}

#[derive(Debug, Clone)]
pub struct Progn {
    pub body: Vec<Expr>,
}

impl Progn {
    pub fn new(body: Vec<Expr>) -> Self {
        Self { body }
    }
}

pub fn new_box(expr: Expr) -> BoxExpr {
    Box::new(expr)
}

#[derive(Clone, Debug, Copy)]
pub enum Var {
    Resolved(Symbol),
    Global(Symbol),
    Local(Ident),
    Captured(Ident),
    Argument(Ident),
    Unresolved(Ident),
}

impl From<Var> for Ident {
    fn from(var: Var) -> Self {
        match var {
            Var::Resolved(symbol) => symbol.into(),
            Var::Global(symbol) => symbol.into(),
            Var::Local(ident) => ident,
            Var::Captured(cap) => cap,
            Var::Argument(arg) => arg,
            Var::Unresolved(ident) => ident,
        }
    }
}

impl From<Var> for Symbol {
    fn from(var: Var) -> Self {
        match var {
            Var::Resolved(symbol) => symbol,
            Var::Global(symbol) => symbol,
            Var::Local(ident) => ident.into(),
            Var::Captured(ident) => ident.into(),
            Var::Argument(ident) => ident.into(),
            Var::Unresolved(ident) => ident.into(),
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
    And(Vec<Expr>),
    Or(Vec<Expr>),
    If(If),
    Catch(Catch),
    Cond(Cond),
    ConditionCase(ConditionCase),
    Defconst(Defconst),
    Defvar(Defvar),
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

pub type Bindings = Vec<(Arg, Option<Expr>)>;

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Rc<Bindings>,
    pub body: Progn,
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct LetStar {
    pub bindings: Rc<Bindings>,
    pub body: Progn,
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: BoxExpr,
    pub then: BoxExpr,
    pub els: Progn,
}

#[derive(Debug, Clone)]
pub struct Args {
    pub normal: Vec<Arg>,
    pub optional: Option<Vec<Arg>>,
    pub rest: Option<Arg>,
}

impl Args {
    pub fn fixed_param_count(&self) -> usize {
        let optional = self.optional.as_ref().map_or(0, |args| args.len());
        self.normal.len() + optional
    }

    pub fn use_argv_abi(&self) -> bool {
        self.rest.is_some() || self.fixed_param_count() > 8
    }
}

pub type Captures = Rc<RefCell<Vec<Var>>>;

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
    pub captures: Captures,
}

#[derive(Debug, Clone)]
pub struct Interactive {
    pub arg_desc: Option<String>,
    pub modes: Option<Vec<CellVar>>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub symbol: CellVar,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub symbol: CellVar,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Setq {
    pub assignments: Vec<(CellVar, Expr)>,
}

#[derive(Debug, Clone)]
pub struct Defvar {
    pub symbol: Ident,
    pub value: Option<BoxExpr>,
    pub docstring: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Defconst {
    pub symbol: Ident,
    pub value: BoxExpr,
    pub docstring: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub clauses: Vec<CondClause>,
}

#[derive(Debug, Clone)]
pub struct CondClause {
    pub condition: Expr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: BoxExpr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct Prog1 {
    pub first: BoxExpr,
    pub rest: Progn,
}

#[derive(Debug, Clone)]
pub struct Prog2 {
    pub first: BoxExpr,
    pub second: BoxExpr,
    pub rest: Progn,
}

#[derive(Debug, Clone)]
pub struct Catch {
    pub tag: BoxExpr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct UnwindProtect {
    pub protected: BoxExpr,
    pub cleanup: Progn,
}

#[derive(Debug, Clone)]
pub struct ConditionCase {
    pub var: Option<Ident>,
    pub protected: BoxExpr,
    pub handlers: Vec<ConditionHandler>,
}

#[derive(Debug, Clone)]
pub struct ConditionHandler {
    pub condition: Expr,
    pub body: Progn,
}

#[derive(Debug, Clone)]
pub struct SetqDefault {
    pub assignments: Vec<(CellVar, Expr)>,
}

#[derive(Debug, Clone, Eq)]
pub struct Arg {
    pub ident: Ident,
    pub captured: Cell<usize>,
}

impl std::hash::Hash for Arg {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
    }
}

impl PartialEq for Arg {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Arg {
    pub fn new(ident: Ident, captured: usize) -> Self {
        Self {
            ident,
            captured: Cell::new(captured),
        }
    }
    pub fn new_uncap(ident: Ident) -> Self {
        Self::new(ident, 0)
    }

    pub fn is_shared(&self) -> bool {
        self.captured.get() > 1
    }
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
    Function,  // (function expr)
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
    Unquote(BoxExpr),       // ,expr
    UnquoteSplice(BoxExpr), // ,@expr
}

#[derive(Clone, Debug, PartialEq)]
pub enum Number {
    Integer(LispInteger),
    Real(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(Number),
    Character(LispCharacter),
    String(String),
}

impl ExprType {
    pub fn new_int(n: i64) -> Self {
        Self::Literal(Literal::Number(Number::Integer(LispInteger::new(n))))
    }

    pub fn new_float(f: f64) -> Self {
        Self::Literal(Literal::Number(Number::Real(f)))
    }

    pub fn new_char(c: char) -> Self {
        Self::Literal(Literal::Character(LispCharacter::new(c)))
    }

    pub fn new_str(str: String) -> Self {
        Self::Literal(Literal::String(str))
    }
}

impl Literal {
    pub fn to_object(&self, ctx: &mut crate::gc::GcContext) -> crate::core::object::Object {
        match self {
            Self::Number(Number::Integer(int)) => int.clone().tag(),
            Self::Number(Number::Real(float)) => LispFloat::new(*float, ctx).tag(),
            Self::Character(char) => char.clone().tag(),
            Self::String(string) => LispStr::new(string.clone(), ctx).tag(),
        }
    }
}

impl From<Var> for LispObject {
    fn from(var: Var) -> Self {
        match var {
            Var::Resolved(symbol) => LispObject::Symbol(LispSymbol(symbol)),
            Var::Global(symbol) => LispObject::Symbol(LispSymbol(symbol)),
            Var::Local(ident) => LispObject::Symbol(LispSymbol(ident.into())),
            Var::Captured(ident) => LispObject::Symbol(LispSymbol(ident.into())),
            Var::Argument(ident) => LispObject::Symbol(LispSymbol(ident.into())),
            Var::Unresolved(ident) => LispObject::Symbol(LispSymbol(ident.into())),
        }
    }
}

/// Owned syntax data avoids keeping GC handles in the parser or macro input tree.
#[derive(Clone, Debug)]
pub(crate) enum Datum {
    Nil,
    Symbol(Symbol),
    Literal(Literal),
    List(Vec<Datum>),
    Vector(Vec<Datum>),
}

impl Datum {
    fn symbol(name: impl Into<Symbol>) -> Self {
        Self::Symbol(name.into())
    }
    fn form(name: &str, items: impl IntoIterator<Item = Self>) -> Self {
        Self::List(std::iter::once(Self::symbol(name)).chain(items).collect())
    }
    fn exprs(exprs: &[Expr]) -> Vec<Self> {
        exprs.iter().map(Expr::to_datum).collect()
    }
    fn bindings(bindings: &Bindings) -> Self {
        Self::List(
            bindings
                .iter()
                .map(|(arg, value)| match value {
                    Some(value) => Self::List(vec![Self::symbol(arg.ident), value.to_datum()]),
                    None => Self::symbol(arg.ident),
                })
                .collect(),
        )
    }
    fn string(string: &str) -> Self {
        Self::Literal(Literal::String(string.to_owned()))
    }
    pub(crate) fn to_object(&self, ctx: &mut crate::gc::GcContext) -> crate::core::object::Object {
        match self {
            Self::Nil => nil(),
            Self::Symbol(symbol) => LispSymbol(*symbol).tag(),
            Self::Literal(literal) => literal.to_object(ctx),
            Self::List(items) | Self::Vector(items) => {
                let mut roots = Vec::with_capacity(items.len());
                for item in items {
                    let value = item.to_object(ctx);
                    roots.push(ctx.root(value));
                }
                let values: Vec<_> = roots.iter().map(|root| *root.get(ctx)).collect();
                let values = ctx.root_slice(&values);
                if matches!(self, Self::List(_)) {
                    LispCons::list_from_rooted_slice(&values, ctx)
                } else {
                    LispVector(ctx.alloc::<crate::core::vector::Vector>(&values)).tag()
                }
            }
        }
    }
}

impl Expr {
    pub fn to_object(&self, ctx: &mut crate::gc::GcContext) -> crate::core::object::Object {
        self.to_datum().to_object(ctx)
    }
    fn to_datum(&self) -> Datum {
        match &*self.ty() {
            ExprType::Nil => Datum::Nil,
            ExprType::Literal(lit) => Datum::Literal(lit.clone()),
            ExprType::Symbol(var) => Datum::symbol(Symbol::from(var.get())),
            ExprType::Vector(items) => Datum::Vector(Datum::exprs(items)),
            ExprType::Call(call) => Datum::form(
                Symbol::from(call.symbol.get()).name(),
                Datum::exprs(&call.args),
            ),
            ExprType::SpecialForm(form) => form.to_datum(),
        }
    }
}

impl QuotedData {
    fn to_datum(&self) -> Datum {
        match self {
            Self::Literal(lit) => Datum::Literal(lit.clone()),
            Self::Symbol(var) => Datum::symbol(Symbol::from(var.get())),
            Self::List(items) => Datum::List(items.iter().map(Self::to_datum).collect()),
            Self::Vector(items) => Datum::Vector(items.iter().map(Self::to_datum).collect()),
            Self::Unquote(expr) => Datum::form("unquote", [expr.to_datum()]),
            Self::UnquoteSplice(expr) => Datum::form("unquote-splicing", [expr.to_datum()]),
        }
    }
}

impl SpecialForm {
    fn to_datum(&self) -> Datum {
        use Datum as D;
        let (name, items) = match self {
            Self::And(exprs) => ("and", D::exprs(exprs)),
            Self::Or(exprs) => ("or", D::exprs(exprs)),
            Self::Progn(p) => ("progn", D::exprs(&p.body)),
            Self::SaveCurrentBuffer(p) => ("save-current-buffer", D::exprs(&p.body)),
            Self::SaveExcursion(p) => ("save-excursion", D::exprs(&p.body)),
            Self::SaveRestriction(p) => ("save-restriction", D::exprs(&p.body)),
            Self::If(e) => (
                "if",
                [
                    vec![e.cond.to_datum(), e.then.to_datum()],
                    D::exprs(&e.els.body),
                ]
                .concat(),
            ),
            Self::Let(e) => (
                "let",
                [vec![D::bindings(&e.bindings)], D::exprs(&e.body.body)].concat(),
            ),
            Self::LetStar(e) => (
                "let*",
                [vec![D::bindings(&e.bindings)], D::exprs(&e.body.body)].concat(),
            ),
            Self::Quote(e) => (
                match e.kind {
                    QuoteKind::Quote => "quote",
                    QuoteKind::Backquote => "backquote",
                    QuoteKind::Function => "function",
                },
                vec![e.expr.to_datum()],
            ),
            Self::Catch(e) => (
                "catch",
                [vec![e.tag.to_datum()], D::exprs(&e.body.body)].concat(),
            ),
            Self::Cond(e) => (
                "cond",
                e.clauses
                    .iter()
                    .map(|c| {
                        D::List([vec![c.condition.to_datum()], D::exprs(&c.body.body)].concat())
                    })
                    .collect(),
            ),
            Self::ConditionCase(e) => {
                let mut items = vec![
                    e.var.map(D::symbol).unwrap_or(D::Nil),
                    e.protected.to_datum(),
                ];
                items.extend(e.handlers.iter().map(|h| {
                    D::List([vec![h.condition.to_datum()], D::exprs(&h.body.body)].concat())
                }));
                ("condition-case", items)
            }
            Self::Defvar(e) => {
                let mut items = vec![D::symbol(e.symbol)];
                if let Some(value) = &e.value {
                    items.push(value.to_datum());
                }
                if let Some(doc) = &e.docstring {
                    items.push(D::string(doc));
                }
                ("defvar", items)
            }
            Self::Defconst(e) => {
                let mut items = vec![D::symbol(e.symbol), e.value.to_datum()];
                if let Some(doc) = &e.docstring {
                    items.push(D::string(doc));
                }
                ("defconst", items)
            }
            Self::Lambda(e) => {
                let mut args: Vec<_> = e.args.normal.iter().map(|a| D::symbol(a.ident)).collect();
                if let Some(optional) = &e.args.optional {
                    args.push(D::symbol("&optional"));
                    args.extend(optional.iter().map(|a| D::symbol(a.ident)));
                }
                if let Some(rest) = &e.args.rest {
                    args.extend([D::symbol("&rest"), D::symbol(rest.ident)]);
                }
                let mut items = vec![D::List(args)];
                if let Some(doc) = &e.docstring {
                    items.push(D::string(doc));
                }
                if let Some(interactive) = &e.interactive {
                    items.push(Self::Interactive(interactive.clone()).to_datum());
                }
                if let Some(declare) = &e.declare {
                    items.push(D::form("declare", D::exprs(declare)));
                }
                items.extend(D::exprs(&e.body.body));
                ("lambda", items)
            }
            Self::Interactive(e) => {
                let mut items = Vec::new();
                if let Some(desc) = &e.arg_desc {
                    items.push(D::string(desc));
                }
                if let Some(modes) = &e.modes {
                    items.extend(modes.iter().map(|v| D::symbol(Symbol::from(v.get()))));
                }
                ("interactive", items)
            }
            Self::Prog1(e) => (
                "prog1",
                [vec![e.first.to_datum()], D::exprs(&e.rest.body)].concat(),
            ),
            Self::Prog2(e) => (
                "prog2",
                [
                    vec![e.first.to_datum(), e.second.to_datum()],
                    D::exprs(&e.rest.body),
                ]
                .concat(),
            ),
            Self::Set(e) => (
                "set",
                vec![D::symbol(Symbol::from(e.symbol.get())), e.value.to_datum()],
            ),
            Self::Setq(e) => (
                "setq",
                e.assignments
                    .iter()
                    .flat_map(|(v, e)| [D::symbol(Symbol::from(v.get())), e.to_datum()])
                    .collect(),
            ),
            Self::SetqDefault(e) => (
                "setq-default",
                e.assignments
                    .iter()
                    .flat_map(|(v, e)| [D::symbol(Symbol::from(v.get())), e.to_datum()])
                    .collect(),
            ),
            Self::UnwindProtect(e) => (
                "unwind-protect",
                [vec![e.protected.to_datum()], D::exprs(&e.cleanup.body)].concat(),
            ),
            Self::While(e) => (
                "while",
                [vec![e.condition.to_datum()], D::exprs(&e.body.body)].concat(),
            ),
        };
        D::form(name, items)
    }
}
