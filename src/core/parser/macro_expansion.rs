use std::rc::Rc;

use crate::core::{
    cons::Cons,
    env::{Environment, FuncCellType},
    error::{RuntimeError, RuntimeResult},
    object::{LispObject, ObjectRef},
    parser::{
        expr::{Call, Expr, ExprType, QuotedData, SpecialForm, Var},
        scope::Scope,
        token::Token,
    },
};
use chumsky::{
    input::{Input, Stream},
    Parser,
};

/// Expand all macros in an expression recursively until no more expansions are possible
pub fn macro_expand_all(expr: &Expr, env: &Environment) -> RuntimeResult<()> {
    let scope = Scope::Global(env);
    macro_expand_all_with_scope(expr, &scope, env)
}

/// Expand macros once (macroexpand-1 equivalent)
pub fn macro_expand_once(expr: &Expr, env: &Environment) -> RuntimeResult<bool> {
    let scope = Scope::Global(env);
    macro_expand_once_with_scope(expr, &scope, env)
}

/// Internal function for recursive macro expansion with scope tracking
fn macro_expand_all_with_scope(expr: &Expr, scope: &Scope, env: &Environment) -> RuntimeResult<()> {
    // Keep expanding until no more changes occur
    loop {
        let expanded = macro_expand_once_with_scope(expr, scope, env)?;

        // If no expansion occurred, recursively expand subexpressions and break
        if !expanded {
            expand_subexpressions(expr, scope, env)?;
            break;
        }
        // Continue expanding if changes occurred
    }
    Ok(())
}

/// Expand macros once with scope tracking
/// Returns true if expansion occurred, false otherwise
fn macro_expand_once_with_scope(
    expr: &Expr,
    scope: &Scope,
    env: &Environment,
) -> RuntimeResult<bool> {
    let expr_type = expr.ty.borrow();

    match &*expr_type {
        ExprType::Call(call) => {
            // First resolve the function symbol in the current scope
            let resolved_var = scope.resolve(call.symbol.get().into());
            call.symbol.set(resolved_var);

            // Check if this is a macro call
            if let Some(expanded_expr_type) = try_expand_macro_call(call, resolved_var, env)? {
                drop(expr_type);
                // Replace the expression content
                expr.ty.replace(expanded_expr_type);
                Ok(true)
            } else {
                // Not a macro, no expansion
                Ok(false)
            }
        }
        ExprType::SpecialForm(_) => {
            // Handle special forms that might contain macro calls
            drop(expr_type);
            expand_special_form_macros(expr, scope, env)
        }
        _ => {
            // Literals, symbols, vectors, etc. - no macro expansion needed
            Ok(false)
        }
    }
}

/// Recursively expand subexpressions after top-level expansion is complete
fn expand_subexpressions(expr: &Expr, scope: &Scope, env: &Environment) -> RuntimeResult<()> {
    let expr_type = expr.ty.borrow();

    match &*expr_type {
        ExprType::Vector(exprs) => {
            for expr in exprs.iter() {
                macro_expand_all_with_scope(expr, scope, env)?;
            }
        }
        ExprType::Call(call) => {
            // Expand arguments
            for arg in call.args.iter() {
                macro_expand_all_with_scope(arg, scope, env)?;
            }
        }
        ExprType::SpecialForm(special_form) => {
            expand_special_form_subexpressions(expr, special_form, scope, env)?;
        }
        ExprType::Symbol(sym) => {
            let resolved_var = scope.resolve(sym.get().into());
            sym.set(resolved_var);
        }
        _ => {
            // No subexpressions to expand
        }
    }

    Ok(())
}

/// Try to expand a macro call, returning Some(expanded_expr_type) if it's a macro, None otherwise
fn try_expand_macro_call(
    call: &Call,
    resolved_var: Var,
    env: &Environment,
) -> RuntimeResult<Option<ExprType>> {
    let symbol = match resolved_var {
        Var::Global(symbol) => symbol,
        _ => return Ok(None), // Only global symbols can be macros
    };

    // Try to load as a macro
    let result = env.load_symbol_with(symbol, Some(FuncCellType::Macro), |val| {
        let ObjectRef::Function(func) = val.as_ref() else {
            return Ok(None);
        };

        // Convert arguments to LispObjects (unevaluated for macros)
        let args = call
            .args
            .iter()
            .map(|arg| {
                let obj: LispObject = arg.clone().into();
                obj.tag()
            })
            .collect::<Vec<_>>();

        // Execute the macro
        let result = func.run(&args, env)?;

        // Convert result back to ExprType
        let tokens = lisp_object_to_tokens(result.untag());
        let expanded_expr = parse_tokens_to_expr(tokens)?;

        Ok(Some(expanded_expr.ty.into_inner()))
    });

    match result {
        Ok(Some(expanded)) => Ok(Some(expanded)),
        Ok(None) => Ok(None),
        Err(_) => Ok(None), // Symbol not found or not a macro
    }
}

/// Handle macro expansion in special forms
/// Returns true if expansion occurred, false otherwise
fn expand_special_form_macros(
    expr: &Expr,
    scope: &Scope,
    env: &Environment,
) -> RuntimeResult<bool> {
    let expr_type = expr.ty.borrow();

    match &*expr_type {
        ExprType::SpecialForm(special_form) => {
            match special_form {
                SpecialForm::Let(_) => {
                    drop(expr_type);
                    expand_let_macros(expr, scope, env)
                }
                SpecialForm::LetStar(_) => {
                    drop(expr_type);
                    expand_let_star_macros(expr, scope, env)
                }
                SpecialForm::Lambda(_) => {
                    drop(expr_type);
                    expand_lambda_macros(expr, scope, env)
                }
                _ => {
                    // Other special forms don't introduce new scopes for macro expansion
                    Ok(false)
                }
            }
        }
        _ => Ok(false),
    }
}

/// Expand macros in let bindings and body with proper scope
fn expand_let_macros(expr: &Expr, parent_scope: &Scope, env: &Environment) -> RuntimeResult<bool> {
    let mut expr_type = expr.ty.borrow_mut();

    if let ExprType::SpecialForm(SpecialForm::Let(let_expr)) = &mut *expr_type {
        // Create new lexical scope for the let binding
        let new_scope = Scope::Lexical {
            binding: let_expr.bindings.clone(),
            parent: Box::new(parent_scope.clone()),
        };

        // Expand value expressions in the parent scope (before binding)
        for (_, value) in let_expr.bindings.iter() {
            if let Some(val_expr) = value {
                macro_expand_all_with_scope(val_expr, parent_scope, env)?;
            }
        }

        // Expand body in the new scope (after binding)
        for body_expr in let_expr.body.body.iter() {
            macro_expand_all_with_scope(body_expr, &new_scope, env)?;
        }
    }

    Ok(false) // Let forms themselves don't expand, only their contents
}

/// Expand macros in let* bindings and body with incremental scope
fn expand_let_star_macros(
    expr: &Expr,
    parent_scope: &Scope,
    env: &Environment,
) -> RuntimeResult<bool> {
    let mut expr_type = expr.ty.borrow_mut();

    if let ExprType::SpecialForm(SpecialForm::LetStar(let_star)) = &mut *expr_type {
        let mut current_scope = parent_scope.clone();

        // For let*, each binding sees the previous bindings
        for (i, (_, value)) in let_star.bindings.iter().enumerate() {
            if let Some(val_expr) = value {
                macro_expand_all_with_scope(val_expr, &current_scope, env)?;
            }

            // Create new scope that includes this binding for subsequent bindings
            if i < let_star.bindings.len() - 1 {
                let partial_bindings: Vec<_> =
                    let_star.bindings.iter().take(i + 1).cloned().collect();
                current_scope = Scope::Lexical {
                    binding: Rc::new(partial_bindings),
                    parent: Box::new(current_scope),
                };
            }
        }

        // Create final scope for body
        let final_scope = Scope::Lexical {
            binding: let_star.bindings.clone(),
            parent: Box::new(parent_scope.clone()),
        };

        // Expand body in the final scope
        for body_expr in let_star.body.body.iter() {
            macro_expand_all_with_scope(body_expr, &final_scope, env)?;
        }
    }

    Ok(false) // Let* forms themselves don't expand, only their contents
}

/// Expand macros in lambda body with function scope
fn expand_lambda_macros(
    expr: &Expr,
    parent_scope: &Scope,
    env: &Environment,
) -> RuntimeResult<bool> {
    let mut expr_type = expr.ty.borrow_mut();

    if let ExprType::SpecialForm(SpecialForm::Lambda(lambda)) = &mut *expr_type {
        // Create function scope
        let function_scope = Scope::Function {
            args: lambda.args.clone(),
            captures: lambda.captures.clone(),
            parent: Box::new(parent_scope.clone()),
        };

        // Expand body in function scope
        for body_expr in lambda.body.body.iter() {
            macro_expand_all_with_scope(body_expr, &function_scope, env)?;
        }
    }

    Ok(false) // Lambda forms themselves don't expand, only their contents
}

/// Expand subexpressions in special forms
fn expand_special_form_subexpressions(
    expr: &Expr,
    special_form: &SpecialForm,
    scope: &Scope,
    env: &Environment,
) -> RuntimeResult<()> {
    match special_form {
        SpecialForm::If(if_expr) => {
            macro_expand_all_with_scope(&if_expr.cond, scope, env)?;
            macro_expand_all_with_scope(&if_expr.then, scope, env)?;

            for else_expr in if_expr.els.body.iter() {
                macro_expand_all_with_scope(else_expr, scope, env)?;
            }
        }
        SpecialForm::While(while_expr) => {
            macro_expand_all_with_scope(&while_expr.condition, scope, env)?;

            for body_expr in while_expr.body.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::Let(_) => {
            // Already handled in expand_let_macros
            expand_let_macros(expr, scope, env)?;
        }
        SpecialForm::LetStar(_) => {
            // Already handled in expand_let_star_macros
            expand_let_star_macros(expr, scope, env)?;
        }
        SpecialForm::Lambda(_) => {
            // Already handled in expand_lambda_macros
            expand_lambda_macros(expr, scope, env)?;
        }
        SpecialForm::And(exprs) => {
            for expr in exprs.iter() {
                macro_expand_all_with_scope(expr, scope, env)?;
            }
        }
        SpecialForm::Or(exprs) => {
            for expr in exprs.iter() {
                macro_expand_all_with_scope(expr, scope, env)?;
            }
        }
        SpecialForm::Catch(catch_expr) => {
            macro_expand_all_with_scope(&catch_expr.tag, scope, env)?;

            for body_expr in catch_expr.body.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::Cond(cond_expr) => {
            for clause in cond_expr.clauses.iter() {
                macro_expand_all_with_scope(&clause.condition, scope, env)?;

                for body_expr in clause.body.body.iter() {
                    macro_expand_all_with_scope(body_expr, scope, env)?;
                }
            }
        }
        SpecialForm::ConditionCase(condition_case) => {
            macro_expand_all_with_scope(&condition_case.protected, scope, env)?;

            for handler in condition_case.handlers.iter() {
                macro_expand_all_with_scope(&handler.condition, scope, env)?;

                for body_expr in handler.body.body.iter() {
                    macro_expand_all_with_scope(body_expr, scope, env)?;
                }
            }
        }
        SpecialForm::Defvar(defvar) => {
            if let Some(value) = &defvar.value {
                macro_expand_all_with_scope(value, scope, env)?;
            }
        }
        SpecialForm::Defconst(defconst) => {
            macro_expand_all_with_scope(&defconst.value, scope, env)?;
        }
        SpecialForm::Prog1(prog1) => {
            macro_expand_all_with_scope(&prog1.first, scope, env)?;

            for body_expr in prog1.rest.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::Prog2(prog2) => {
            macro_expand_all_with_scope(&prog2.first, scope, env)?;
            macro_expand_all_with_scope(&prog2.second, scope, env)?;

            for body_expr in prog2.rest.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::Progn(progn) => {
            for body_expr in progn.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::Set(set) => {
            let resolved = scope.resolve(set.symbol.get().into());
            set.symbol.set(resolved);
            macro_expand_all_with_scope(&set.value, scope, env)?;
        }
        SpecialForm::Setq(setq) => {
            for (symbol, expr) in setq.assignments.iter() {
                let resolved = scope.resolve(symbol.get().into());
                symbol.set(resolved);
                macro_expand_all_with_scope(expr, scope, env)?;
            }
        }
        SpecialForm::SetqDefault(setq_default) => {
            for (_, expr) in setq_default.assignments.iter() {
                macro_expand_all_with_scope(expr, scope, env)?;
            }
        }
        SpecialForm::SaveCurrentBuffer(progn) => {
            for body_expr in progn.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::SaveExcursion(progn) => {
            for body_expr in progn.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::SaveRestriction(progn) => {
            for body_expr in progn.body.iter() {
                macro_expand_all_with_scope(body_expr, scope, env)?;
            }
        }
        SpecialForm::UnwindProtect(unwind_protect) => {
            macro_expand_all_with_scope(&unwind_protect.protected, scope, env)?;

            for cleanup_expr in unwind_protect.cleanup.body.iter() {
                macro_expand_all_with_scope(cleanup_expr, scope, env)?;
            }
        }
        SpecialForm::Quote(quote) => {
            // Quote forms contain quoted data, which should not be macro-expanded
            // The quoted expressions are not evaluated, so no macro expansion needed
            expand_quoted_data(&quote.expr, scope, env)?;
        }
        SpecialForm::Interactive(interactive) => {
            // Interactive forms may contain expressions in modes
            if let Some(modes) = &interactive.modes {
                for mode_var in modes.iter() {
                    let resolved_var = scope.resolve(mode_var.get().into());
                    mode_var.set(resolved_var);
                }
            }
        }
    }

    Ok(())
}

/// Parse tokens back into an expression
fn parse_tokens_to_expr(tokens: Vec<Token>) -> RuntimeResult<Expr> {
    let tokens_with_span = tokens
        .into_iter()
        .map(|tok| (tok, crate::core::parser::Span::dummy()));
    let stream = Stream::from_iter(tokens_with_span)
        .map(crate::core::parser::Span::dummy(), |(t, s)| (t, s));

    match crate::core::parser::expr_parser::expr_parser()
        .parse(stream)
        .into_result()
    {
        Ok(expr) => Ok(expr),
        Err(parse_errors) => Err(RuntimeError::MacroExpansionError {
            message: format!("Failed to parse macro expansion result: {:?}", parse_errors),
        }),
    }
}

/// Expand macros in quoted data (for backquote/unquote handling)
fn expand_quoted_data(data: &QuotedData, scope: &Scope, env: &Environment) -> RuntimeResult<()> {
    match data {
        QuotedData::Literal(_) => {
            // Literals don't need expansion
        }
        QuotedData::Symbol(_var) => {
            // DONT Resolve symbols in quoted context
        }
        QuotedData::List(items) => {
            for item in items.iter() {
                expand_quoted_data(item, scope, env)?;
            }
        }
        QuotedData::Vector(items) => {
            for item in items.iter() {
                expand_quoted_data(item, scope, env)?;
            }
        }
        QuotedData::Unquote(expr) => {
            // Unquoted expressions should be macro-expanded
            macro_expand_all_with_scope(expr, scope, env)?;
        }
        QuotedData::UnquoteSplice(expr) => {
            // Unquote-splice expressions should be macro-expanded
            macro_expand_all_with_scope(expr, scope, env)?;
        }
    }
    Ok(())
}

/// Legacy function - now redirects to the new macro expansion system
pub fn macro_expand_call(call: &Call, env: &Environment) -> Option<RuntimeResult<Expr>> {
    let symbol = call.symbol.get().into();
    let result = env
        .load_symbol_with(symbol, Some(FuncCellType::Macro), |val| {
            let ObjectRef::Function(func) = val.as_ref() else {
                return Ok(Err(RuntimeError::WrongType {
                    expected: "function",
                    actual: (val.get_tag()),
                }));
            };
            let args = call
                .args
                .iter()
                .map(|arg| {
                    let obj: LispObject = arg.clone().into();
                    obj.tag()
                })
                .collect::<Vec<_>>();

            let result = func.run(&args, env).and_then(|result| {
                // Convert the macro result to tokens
                let tokens = lisp_object_to_tokens(result.untag());

                // Parse the tokens back into an expression
                let tokens_with_span = tokens
                    .into_iter()
                    .map(|tok| (tok, crate::core::parser::Span::dummy()));
                let stream = Stream::from_iter(tokens_with_span)
                    .map(crate::core::parser::Span::dummy(), |(t, s)| (t, s));

                match crate::core::parser::expr_parser::expr_parser()
                    .parse(stream)
                    .into_result()
                {
                    Ok(expr) => Ok(expr),
                    Err(parse_errors) => Err(RuntimeError::MacroExpansionError {
                        message: format!(
                            "Failed to parse macro expansion result: {:?}",
                            parse_errors
                        ),
                    }),
                }
            });
            Ok(result)
        })
        .ok();

    result
}

/// Convert a LispObject into a sequence of tokens for macro expansion
pub fn lisp_object_to_tokens(obj: LispObject) -> Vec<Token> {
    match obj {
        LispObject::Nil => vec![Token::Ident("nil".into())],
        LispObject::True => vec![Token::Ident("t".into())],
        LispObject::Int(lisp_integer) => vec![Token::Integer(lisp_integer.0)],
        LispObject::Float(lisp_float) => vec![Token::Float(lisp_float.0)],
        LispObject::Character(lisp_character) => vec![Token::Character(lisp_character.value())],
        LispObject::Str(lisp_str) => vec![Token::Str(lisp_str.to_string())],
        LispObject::Symbol(lisp_symbol) => vec![Token::Ident(lisp_symbol.0.ident())],
        LispObject::Vector(lisp_vector) => {
            let mut tokens = vec![Token::LBracket];
            for item in lisp_vector.0.get() {
                let item_obj = item.clone().untag();
                tokens.extend(lisp_object_to_tokens(item_obj));
            }
            tokens.push(Token::RBracket);
            tokens
        }
        LispObject::Cons(lisp_cons) => cons_to_tokens(&lisp_cons.0.get()),
        LispObject::Function(_) => {
            // Functions can't be directly converted to tokens for macro expansion
            vec![Token::Ident("#<function>".into())]
        }
        LispObject::HashTable(_) => {
            // Hash tables can't be directly converted to tokens for macro expansion
            vec![Token::Ident("#<hash-table>".into())]
        }
    }
}

/// Convert a cons cell to tokens, handling proper list structure
fn cons_to_tokens(cons: &Cons) -> Vec<Token> {
    let mut tokens = vec![Token::LParen];

    let vec = cons.to_vec().unwrap();
    // Handle the car
    tokens.push(Token::LParen);
    for obj in vec.into_iter() {
        tokens.extend(lisp_object_to_tokens(obj.untag()));
    }
    tokens.push(Token::RParen);

    tokens
}

/// Iterator adapter for streaming tokens from a LispObject
pub struct LispObjectTokenIterator {
    tokens: std::vec::IntoIter<Token>,
}

impl LispObjectTokenIterator {
    pub fn new(obj: LispObject) -> Self {
        Self {
            tokens: lisp_object_to_tokens(obj).into_iter(),
        }
    }
}

impl Iterator for LispObjectTokenIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}

impl From<LispObject> for LispObjectTokenIterator {
    fn from(obj: LispObject) -> Self {
        Self::new(obj)
    }
}
