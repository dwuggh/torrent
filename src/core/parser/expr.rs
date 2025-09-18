use crate::core::{
    cons::{Cons, LispCons},
    ident::Ident,
    number::{LispCharacter, LispFloat, LispInteger},
    object::{nil, LispObject},
    parser::Span,
    string::LispStr,
    symbol::{LispSymbol, Symbol},
    tagged_ptr::TaggedObj,
    vector::LispVector,
};
use crate::gc::Gc;
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
    And(Vec<Expr>),
    Or(Vec<Expr>),
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
    pub cond: BoxExpr,
    pub then: BoxExpr,
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
pub struct Function {
    pub body: Progn,
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
    Unquote(BoxExpr),       // ,expr
    UnquoteSplice(BoxExpr), // ,@expr
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

impl From<Literal> for LispObject {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Number(Number::Integer(int)) => LispObject::Int(int),
            Literal::Number(Number::Real(float)) => LispObject::Float(float),
            Literal::Character(char) => LispObject::Character(char),
            Literal::String(string) => LispObject::Str(string),
        }
    }
}

impl From<Var> for LispObject {
    fn from(var: Var) -> Self {
        match var {
            Var::Symbol(symbol) => LispObject::Symbol(LispSymbol(symbol)),
            Var::Local(local) => LispObject::Symbol(LispSymbol(local.ident.into())),
        }
    }
}

impl From<Expr> for LispObject {
    fn from(expr: Expr) -> Self {
        let expr_type = expr.ty.into_inner();
        match expr_type {
            ExprType::Nil => LispObject::Nil,
            ExprType::Literal(literal) => literal.into(),
            ExprType::Symbol(var) => var.get().into(),
            ExprType::Vector(exprs) => {
                let objects: Vec<_> = exprs
                    .into_iter()
                    .map(|e| e.into())
                    .map(|obj: LispObject| obj.tag())
                    .collect();
                LispObject::Vector(LispVector(Gc::new(objects)))
            }
            ExprType::Call(call) => {
                // Convert function call to a list: (function arg1 arg2 ...)
                let mut elements: Vec<LispObject> = Vec::new();
                elements.push(call.symbol.get().into());
                elements.extend(call.args.into_iter().map(|arg| arg.into()));

                // Convert Vec<LispObject> to proper cons list
                if elements.is_empty() {
                    LispObject::Nil
                } else {
                    let mut result = nil();
                    for obj in elements.into_iter().rev() {
                        result = LispCons::new(obj.tag(), result).tag();
                    }
                    result.untag()
                }
            }
            ExprType::SpecialForm(special_form) => {
                // Convert special forms to lists as well
                match special_form {
                    SpecialForm::If(if_expr) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("if"))),
                            (*if_expr.cond).clone().into(),
                            (*if_expr.then).clone().into(),
                        ];
                        // Add else expressions
                        elements.extend(if_expr.els.body.into_iter().map(|e| e.into()));

                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Let(let_expr) => {
                        let mut elements =
                            vec![LispObject::Symbol(LispSymbol(Symbol::from("let")))];

                        // Convert bindings to list of lists
                        let mut bindings_list = nil();
                        for (ident, value) in let_expr.bindings.iter().rev() {
                            let binding = if let Some(val) = value {
                                // (var value)
                                let var_obj = LispObject::Symbol(LispSymbol(Symbol::from(*ident)));
                                let val_obj: LispObject = val.clone().into();
                                LispCons::new(
                                    var_obj.tag(),
                                    LispCons::new(val_obj.tag(), nil()).tag(),
                                )
                                .tag()
                            } else {
                                // just var
                                LispObject::Symbol(LispSymbol(Symbol::from(*ident))).tag()
                            };
                            bindings_list = LispCons::new(binding, bindings_list).tag();
                        }
                        elements.push(bindings_list.untag());

                        // Add body expressions
                        elements.extend(let_expr.body.body.into_iter().map(|e| e.into()));

                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Quote(quote) => {
                        let symbol = match quote.kind {
                            QuoteKind::Quote => "quote",
                            QuoteKind::Backquote => "backquote",
                        };
                        let quoted_obj = quote_data_to_lisp_object(quote.expr);

                        let mut result = nil();
                        result = LispCons::new(quoted_obj.tag(), result).tag();
                        result = LispCons::new(
                            LispObject::Symbol(LispSymbol(Symbol::from(symbol))).tag(),
                            result,
                        )
                        .tag();
                        result.untag()
                    }
                    SpecialForm::And(exprs) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("and")))];
                        elements.extend(exprs.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Or(exprs) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("or")))];
                        elements.extend(exprs.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Catch(catch_expr) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("catch"))),
                            (*catch_expr.tag).clone().into(),
                        ];
                        elements.extend(catch_expr.body.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Cond(cond_expr) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("cond")))];
                        
                        for clause in cond_expr.clauses {
                            // Each clause is (condition body...)
                            let mut clause_elements = vec![clause.condition.into()];
                            clause_elements.extend(clause.body.body.into_iter().map(|e| e.into()));
                            
                            let mut clause_list = nil();
                            for obj in clause_elements.into_iter().rev() {
                                clause_list = LispCons::new(obj.tag(), clause_list).tag();
                            }
                            elements.push(clause_list.untag());
                        }
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::ConditionCase(condition_case) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("condition-case")))];
                        
                        // Add variable (or nil if None)
                        if let Some(var) = condition_case.var {
                            elements.push(LispObject::Symbol(LispSymbol(Symbol::from(var))));
                        } else {
                            elements.push(LispObject::Nil);
                        }
                        
                        // Add protected form
                        elements.push((*condition_case.protected).clone().into());
                        
                        // Add handlers
                        for handler in condition_case.handlers {
                            let mut handler_elements = vec![handler.condition.into()];
                            handler_elements.extend(handler.body.body.into_iter().map(|e| e.into()));
                            
                            let mut handler_list = nil();
                            for obj in handler_elements.into_iter().rev() {
                                handler_list = LispCons::new(obj.tag(), handler_list).tag();
                            }
                            elements.push(handler_list.untag());
                        }
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Defvar(defvar) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("defvar"))),
                            LispObject::Symbol(LispSymbol(Symbol::from(defvar.symbol))),
                        ];
                        
                        if let Some(value) = defvar.value {
                            elements.push((*value).clone().into());
                        }
                        
                        if let Some(docstring) = defvar.docstring {
                            elements.push(LispObject::Str(LispStr::new(docstring)));
                        }
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Defconst(defconst) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("defconst"))),
                            LispObject::Symbol(LispSymbol(Symbol::from(defconst.symbol))),
                            (*defconst.value).clone().into(),
                        ];
                        
                        if let Some(docstring) = defconst.docstring {
                            elements.push(LispObject::Str(LispStr::new(docstring)));
                        }
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Function(function) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("function")))];
                        elements.extend(function.body.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Lambda(lambda) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("lambda")))];
                        
                        // Convert arguments to list
                        let mut args_list = nil();
                        
                        // Add rest argument if present
                        if let Some(rest) = lambda.args.rest {
                            args_list = LispCons::new(
                                LispObject::Symbol(LispSymbol(Symbol::from(rest))).tag(),
                                args_list
                            ).tag();
                            args_list = LispCons::new(
                                LispObject::Symbol(LispSymbol(Symbol::from("&rest"))).tag(),
                                args_list
                            ).tag();
                        }
                        
                        // Add optional arguments if present
                        if let Some(optional) = &lambda.args.optional {
                            for arg in optional.iter().rev() {
                                args_list = LispCons::new(
                                    LispObject::Symbol(LispSymbol(Symbol::from(*arg))).tag(),
                                    args_list
                                ).tag();
                            }
                            args_list = LispCons::new(
                                LispObject::Symbol(LispSymbol(Symbol::from("&optional"))).tag(),
                                args_list
                            ).tag();
                        }
                        
                        // Add normal arguments
                        for arg in lambda.args.normal.iter().rev() {
                            args_list = LispCons::new(
                                LispObject::Symbol(LispSymbol(Symbol::from(*arg))).tag(),
                                args_list
                            ).tag();
                        }
                        
                        elements.push(args_list.untag());
                        
                        // Add docstring if present
                        if let Some(docstring) = lambda.docstring {
                            elements.push(LispObject::Str(LispStr::new(docstring)));
                        }
                        
                        // Add interactive form if present
                        if let Some(interactive) = lambda.interactive {
                            let mut interactive_elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("interactive")))];
                            
                            if let Some(arg_desc) = interactive.arg_desc {
                                interactive_elements.push(LispObject::Str(LispStr::new(arg_desc)));
                            }
                            
                            if let Some(modes) = interactive.modes {
                                interactive_elements.extend(modes.into_iter().map(|var| var.get().into()));
                            }
                            
                            let mut interactive_list = nil();
                            for obj in interactive_elements.into_iter().rev() {
                                interactive_list = LispCons::new(obj.tag(), interactive_list).tag();
                            }
                            elements.push(interactive_list.untag());
                        }
                        
                        // Add body
                        elements.extend(lambda.body.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::LetStar(let_star) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("let*")))];
                        
                        // Convert bindings to list of lists
                        let mut bindings_list = nil();
                        for (ident, value) in let_star.bindings.iter().rev() {
                            let binding = if let Some(val) = value {
                                // (var value)
                                let var_obj = LispObject::Symbol(LispSymbol(Symbol::from(*ident)));
                                let val_obj: LispObject = val.clone().into();
                                LispCons::new(
                                    var_obj.tag(),
                                    LispCons::new(val_obj.tag(), nil()).tag(),
                                )
                                .tag()
                            } else {
                                // just var
                                LispObject::Symbol(LispSymbol(Symbol::from(*ident))).tag()
                            };
                            bindings_list = LispCons::new(binding, bindings_list).tag();
                        }
                        elements.push(bindings_list.untag());
                        
                        // Add body expressions
                        elements.extend(let_star.body.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Prog1(prog1) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("prog1"))),
                            (*prog1.first).clone().into(),
                        ];
                        elements.extend(prog1.rest.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Prog2(prog2) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("prog2"))),
                            (*prog2.first).clone().into(),
                            (*prog2.second).clone().into(),
                        ];
                        elements.extend(prog2.rest.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Progn(progn) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("progn")))];
                        elements.extend(progn.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Set(set) => {
                        let elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("set"))),
                            LispObject::Symbol(LispSymbol(Symbol::from(set.symbol))),
                            (*set.value).clone().into(),
                        ];
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Setq(setq) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("setq")))];
                        
                        for (ident, expr) in setq.assignments {
                            elements.push(LispObject::Symbol(LispSymbol(Symbol::from(ident))));
                            elements.push(expr.into());
                        }
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::SetqDefault(setq_default) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("setq-default")))];
                        
                        for (ident, expr) in setq_default.assignments {
                            elements.push(LispObject::Symbol(LispSymbol(Symbol::from(ident))));
                            elements.push(expr.into());
                        }
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::SaveCurrentBuffer(progn) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("save-current-buffer")))];
                        elements.extend(progn.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::SaveExcursion(progn) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("save-excursion")))];
                        elements.extend(progn.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::SaveRestriction(progn) => {
                        let mut elements = vec![LispObject::Symbol(LispSymbol(Symbol::from("save-restriction")))];
                        elements.extend(progn.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::UnwindProtect(unwind_protect) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("unwind-protect"))),
                            (*unwind_protect.protected).clone().into(),
                        ];
                        elements.extend(unwind_protect.cleanup.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::While(while_expr) => {
                        let mut elements = vec![
                            LispObject::Symbol(LispSymbol(Symbol::from("while"))),
                            (*while_expr.condition).clone().into(),
                        ];
                        elements.extend(while_expr.body.body.into_iter().map(|e| e.into()));
                        
                        let mut result = nil();
                        for obj in elements.into_iter().rev() {
                            result = LispCons::new(obj.tag(), result).tag();
                        }
                        result.untag()
                    }
                    SpecialForm::Interactive(_) => {
                        // Interactive forms shouldn't appear at top level, but handle gracefully
                        LispObject::Symbol(LispSymbol(Symbol::from("interactive")))
                    }
                }
            }
        }
    }
}

fn quote_data_to_lisp_object(data: QuotedData) -> LispObject {
    match data {
        QuotedData::Literal(literal) => literal.into(),
        QuotedData::Symbol(var) => var.get().into(),
        QuotedData::List(items) => {
            if items.is_empty() {
                LispObject::Nil
            } else {
                let mut result = nil();
                for item in items.into_iter().rev() {
                    let obj = quote_data_to_lisp_object(item);
                    result = LispCons::new(obj.tag(), result).tag();
                }
                result.untag()
            }
        }
        QuotedData::Vector(items) => {
            let objects: Vec<_> = items
                .into_iter()
                .map(quote_data_to_lisp_object)
                .map(|obj| obj.tag())
                .collect();
            LispObject::Vector(LispVector(Gc::new(objects)))
        }
        QuotedData::Unquote(expr) => {
            // Convert unquote to (unquote expr)
            let mut result = nil();
            let car: LispObject = (*expr).clone().into();
            result = LispCons::new(car.tag(), result).tag();
            result = LispCons::new(
                LispObject::Symbol(LispSymbol(Symbol::from(","))).tag(),
                result,
            )
            .tag();
            result.untag()
        }
        QuotedData::UnquoteSplice(expr) => {
            // Convert unquote-splice to (unquote-splicing expr)
            let mut result = nil();
            let car: LispObject = (*expr).clone().into();
            result = LispCons::new(car.tag(), result).tag();
            result = LispCons::new(
                LispObject::Symbol(LispSymbol(Symbol::from(",@"))).tag(),
                result,
            )
            .tag();
            result.untag()
        }
    }
}
