use std::collections::HashMap;

use crate::core::{
    env::Environment,
    ident::Ident,
    parser::expr::*,
    symbol::{Symbol, SymbolSemantics},
};

#[derive(Default)]
struct ConversionScope {
    args: HashMap<Ident, usize>,
    captures: HashMap<Ident, usize>,
}

impl ConversionScope {
    fn from_lambda(lambda: &Lambda) -> Self {
        let mut scope = Self::default();
        let mut index = 0;

        for arg in &lambda.args.normal {
            scope.args.insert(arg.ident, index);
            index += 1;
        }
        if let Some(optional) = &lambda.args.optional {
            for arg in optional {
                scope.args.insert(arg.ident, index);
                index += 1;
            }
        }
        if let Some(rest) = &lambda.args.rest {
            scope.args.insert(rest.ident, index);
        }

        for (index, capture) in lambda.captures.borrow().iter().enumerate() {
            scope.captures.insert(Ident::from(*capture), index);
        }

        scope
    }
}

pub fn convert_closures(expr: &Expr, env: &Environment) {
    convert_expr(expr, env, None);
}

fn convert_expr(expr: &Expr, env: &Environment, scope: Option<&ConversionScope>) {
    match &*expr.ty() {
        ExprType::Nil | ExprType::Literal(_) => {}
        ExprType::Symbol(symbol) => convert_cell(symbol, env, scope),
        ExprType::Vector(exprs) => convert_exprs(exprs, env, scope),
        ExprType::Call(call) => {
            convert_cell(&call.symbol, env, scope);
            convert_exprs(&call.args, env, scope);
        }
        ExprType::SpecialForm(form) => convert_special_form(form, env, scope),
    }
}

fn convert_exprs(exprs: &[Expr], env: &Environment, scope: Option<&ConversionScope>) {
    for expr in exprs {
        convert_expr(expr, env, scope);
    }
}

fn convert_cell(cell: &CellVar, env: &Environment, scope: Option<&ConversionScope>) {
    let resolved = match cell.get() {
        Var::Resolved(_) => return,
        Var::Global(symbol) => Var::Resolved(intern_global(symbol, env)),
        Var::Argument(ident) => {
            let Some(index) = scope.and_then(|scope| scope.args.get(&ident).copied()) else {
                return;
            };
            Var::Resolved(Symbol::new(index, SymbolSemantics::Argument))
        }
        Var::Captured(ident) => {
            let Some(index) = scope.and_then(|scope| scope.captures.get(&ident).copied()) else {
                return;
            };
            Var::Resolved(Symbol::new(index, SymbolSemantics::Lexical))
        }
        Var::Local(_) | Var::Unresolved(_) => return,
    };
    cell.set(resolved);
}

fn intern_global(symbol: Symbol, env: &Environment) -> Symbol {
    if symbol.semantics() == SymbolSemantics::Global && symbol.index().is_some() {
        return symbol;
    }

    let mut symbol_map = env.symbol_map.clone();
    symbol_map.as_mut().intern(symbol.ident(), false).0
}

fn convert_special_form(form: &SpecialForm, env: &Environment, scope: Option<&ConversionScope>) {
    match form {
        SpecialForm::And(exprs)
        | SpecialForm::Or(exprs)
        | SpecialForm::SaveCurrentBuffer(Progn { body: exprs })
        | SpecialForm::SaveExcursion(Progn { body: exprs })
        | SpecialForm::SaveRestriction(Progn { body: exprs })
        | SpecialForm::Progn(Progn { body: exprs }) => convert_exprs(exprs, env, scope),
        SpecialForm::If(if_expr) => {
            convert_expr(&if_expr.cond, env, scope);
            convert_expr(&if_expr.then, env, scope);
            convert_exprs(&if_expr.els.body, env, scope);
        }
        SpecialForm::Catch(catch) => {
            convert_expr(&catch.tag, env, scope);
            convert_exprs(&catch.body.body, env, scope);
        }
        SpecialForm::Cond(cond) => {
            for clause in &cond.clauses {
                convert_expr(&clause.condition, env, scope);
                convert_exprs(&clause.body.body, env, scope);
            }
        }
        SpecialForm::ConditionCase(condition_case) => {
            convert_expr(&condition_case.protected, env, scope);
            for handler in &condition_case.handlers {
                convert_expr(&handler.condition, env, scope);
                convert_exprs(&handler.body.body, env, scope);
            }
        }
        SpecialForm::Defconst(defconst) => convert_expr(&defconst.value, env, scope),
        SpecialForm::Defvar(defvar) => {
            if let Some(value) = &defvar.value {
                convert_expr(value, env, scope);
            }
        }
        SpecialForm::Interactive(interactive) => {
            if let Some(modes) = &interactive.modes {
                for mode in modes {
                    convert_cell(mode, env, scope);
                }
            }
        }
        SpecialForm::Lambda(lambda) => {
            let lambda_scope = ConversionScope::from_lambda(lambda);
            convert_exprs(&lambda.body.body, env, Some(&lambda_scope));
        }
        SpecialForm::Let(let_expr) => {
            for (_, value) in let_expr.bindings.iter() {
                if let Some(value) = value {
                    convert_expr(value, env, scope);
                }
            }
            convert_exprs(&let_expr.body.body, env, scope);
        }
        SpecialForm::LetStar(let_star) => {
            for (_, value) in let_star.bindings.iter() {
                if let Some(value) = value {
                    convert_expr(value, env, scope);
                }
            }
            convert_exprs(&let_star.body.body, env, scope);
        }
        SpecialForm::Prog1(prog1) => {
            convert_expr(&prog1.first, env, scope);
            convert_exprs(&prog1.rest.body, env, scope);
        }
        SpecialForm::Prog2(prog2) => {
            convert_expr(&prog2.first, env, scope);
            convert_expr(&prog2.second, env, scope);
            convert_exprs(&prog2.rest.body, env, scope);
        }
        SpecialForm::Quote(_) => {}
        SpecialForm::Set(set) => {
            convert_cell(&set.symbol, env, scope);
            convert_expr(&set.value, env, scope);
        }
        SpecialForm::Setq(setq) => {
            for (symbol, value) in &setq.assignments {
                convert_cell(symbol, env, scope);
                convert_expr(value, env, scope);
            }
        }
        SpecialForm::SetqDefault(setq_default) => {
            for (symbol, value) in &setq_default.assignments {
                convert_cell(symbol, env, scope);
                convert_expr(value, env, scope);
            }
        }
        SpecialForm::UnwindProtect(unwind_protect) => {
            convert_expr(&unwind_protect.protected, env, scope);
            convert_exprs(&unwind_protect.cleanup.body, env, scope);
        }
        SpecialForm::While(while_expr) => {
            convert_expr(&while_expr.condition, env, scope);
            convert_exprs(&while_expr.body.body, env, scope);
        }
    }
}
