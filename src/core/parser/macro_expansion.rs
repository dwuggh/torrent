use std::rc::Rc;

use crate::core::{
    cons::Cons,
    env::{Environment, FuncCellType},
    error::{RuntimeError, RuntimeResult},
    object::{LispObject, ObjectRef},
    parser::{
        expr::{Call, Expr, ExprType, SpecialForm, Var},
        scope::Scope,
        token::Token,
    },
};
use chumsky::{
    Parser,
    input::{Input, Stream},
};

/// the main function
pub fn expand_and_resolve_everything(expr: &Expr, env: &Environment) -> RuntimeResult<()> {
    let scope = Scope::Global(env);
    expand_and_resolve_everything_inner(expr, &scope, env)
}

fn expand_and_resolve_everything_inner(
    expr: &Expr,
    scope: &Scope,
    env: &Environment,
) -> RuntimeResult<()> {
    match &*expr.ty() {
        ExprType::Nil => Ok(()),
        ExprType::Literal(_literal) => Ok(()),
        ExprType::Symbol(cell) => {
            let resolved = scope.resolve(cell.get().into());
            cell.set(resolved);
            Ok(())
        }
        ExprType::Vector(exprs) => expand_and_resolve_exprs(exprs, scope, env),
        ExprType::Call(call) => {
            let resolved = scope.resolve(call.symbol.get().into());
            call.symbol.set(resolved);
            let expanded = try_expand_macro_call(call, resolved, env)?;
            // is a macro, replace inner with new content
            if let Some(expanded) = expanded {
                expr.ty.replace(expanded);
                expand_and_resolve_everything_inner(expr, scope, env)
            } else {
                for arg in call.args.iter() {
                    expand_and_resolve_everything_inner(arg, scope, env)?;
                }
                Ok(())
            }
        }
        ExprType::SpecialForm(special_form) => {
            match special_form {
                SpecialForm::And(exprs) => expand_and_resolve_exprs(exprs, scope, env),
                SpecialForm::Or(exprs) => expand_and_resolve_exprs(exprs, scope, env),
                SpecialForm::If(if_expr) => {
                    expand_and_resolve_everything_inner(&if_expr.cond, scope, env)?;
                    expand_and_resolve_everything_inner(&if_expr.then, scope, env)?;
                    expand_and_resolve_exprs(&if_expr.els.body, scope, env)
                }
                SpecialForm::Catch(catch) => {
                    expand_and_resolve_everything_inner(&catch.tag, scope, env)?;
                    expand_and_resolve_exprs(&catch.body.body, scope, env)
                }
                SpecialForm::Cond(cond) => {
                    for clause in cond.clauses.iter() {
                        expand_and_resolve_everything_inner(&clause.condition, scope, env)?;
                        expand_and_resolve_exprs(&clause.body.body, scope, env)?;
                    }
                    Ok(())
                }
                SpecialForm::ConditionCase(condition_case) => {
                    // TODO
                    expand_and_resolve_everything_inner(&condition_case.protected, scope, env)?;
                    for handler in condition_case.handlers.iter() {}
                    todo!()
                }
                SpecialForm::Defconst(defconst) => {
                    expand_and_resolve_everything_inner(&defconst.value, scope, env)
                }
                SpecialForm::Defvar(defvar) => {
                    if let Some(expr) = defvar.value.as_ref() {
                        expand_and_resolve_everything_inner(&expr, scope, env)?;
                    }
                    Ok(())
                }
                SpecialForm::Interactive(interactive) => {
                    interactive.modes.as_ref().map(|modes| {
                        for mode in modes.iter() {
                            let resolved = scope.resolve(mode.get().into());
                            mode.set(resolved);
                        }
                    });
                    Ok(())
                }
                SpecialForm::Lambda(lambda) => {
                    let function_scope = Scope::Function {
                        args: lambda.args.clone(),
                        captures: lambda.captures.clone(),
                        parent: Rc::new(scope.clone()),
                    };

                    // Expand body in function scope
                    expand_and_resolve_exprs(&lambda.body.body, &function_scope, env)
                }
                SpecialForm::Let(let_expr) => {
                    let new_scope = Scope::Lexical {
                        bindings: let_expr.bindings.clone(),
                        seq: None,
                        parent: Rc::new(scope.clone()),
                    };
                    for (_, value) in let_expr.bindings.iter() {
                        if let Some(val_expr) = value {
                            expand_and_resolve_everything_inner(val_expr, scope, env)?;
                        }
                    }

                    expand_and_resolve_exprs(&let_expr.body.body, &new_scope, env)
                }
                SpecialForm::LetStar(let_star) => {
                    let parent = Rc::new(scope.clone());
                    for (i, (_, value)) in let_star.bindings.iter().enumerate() {
                        let new_scope = Scope::Lexical {
                            bindings: let_star.bindings.clone(),
                            seq: Some(i),
                            parent: parent.clone(),
                        };
                        if let Some(val_expr) = value {
                            expand_and_resolve_everything_inner(val_expr, &new_scope, env)?;
                        }
                    }
                    let new_scope = Scope::Lexical {
                        bindings: let_star.bindings.clone(),
                        seq: None,
                        parent,
                    };

                    expand_and_resolve_exprs(&let_star.body.body, &new_scope, env)
                }
                SpecialForm::Prog1(prog1) => {
                    expand_and_resolve_everything_inner(&prog1.first, scope, env)?;
                    expand_and_resolve_exprs(&prog1.rest.body, scope, env)
                }
                SpecialForm::Prog2(prog2) => {
                    expand_and_resolve_everything_inner(&prog2.first, scope, env)?;
                    expand_and_resolve_everything_inner(&prog2.second, scope, env)?;
                    expand_and_resolve_exprs(&prog2.rest.body, scope, env)
                }
                SpecialForm::Progn(progn) => expand_and_resolve_exprs(&progn.body, scope, env),
                // TODO quotes
                SpecialForm::Quote(quote) => todo!(),
                SpecialForm::SaveCurrentBuffer(progn) => {
                    expand_and_resolve_exprs(&progn.body, scope, env)
                }
                SpecialForm::SaveExcursion(progn) => {
                    expand_and_resolve_exprs(&progn.body, scope, env)
                }
                SpecialForm::SaveRestriction(progn) => {
                    expand_and_resolve_exprs(&progn.body, scope, env)
                }
                SpecialForm::Set(set) => {
                    let resolved = scope.resolve(set.symbol.get().into());
                    set.symbol.set(resolved);
                    expand_and_resolve_everything_inner(&set.value, scope, env)
                }
                SpecialForm::Setq(setq) => {
                    for (var, val) in setq.assignments.iter() {
                        let resolved = scope.resolve(var.get().into());
                        var.set(resolved);
                        expand_and_resolve_everything_inner(&val, scope, env)?;
                    }
                    Ok(())
                }
                SpecialForm::SetqDefault(setq_default) => {
                    for (var, val) in setq_default.assignments.iter() {
                        let resolved = scope.resolve(var.get().into());
                        var.set(resolved);
                        expand_and_resolve_everything_inner(&val, scope, env)?;
                    }
                    Ok(())
                }
                SpecialForm::UnwindProtect(unwind_protect) => {
                    expand_and_resolve_everything_inner(&unwind_protect.protected, scope, env)?;
                    expand_and_resolve_exprs(&unwind_protect.cleanup.body, scope, env)
                }
                SpecialForm::While(while_expr) => {
                    expand_and_resolve_everything_inner(&while_expr.condition, scope, env)?;
                    expand_and_resolve_exprs(&while_expr.body.body, scope, env)
                }
            }
        }
    }
}

fn expand_and_resolve_exprs(exprs: &[Expr], scope: &Scope, env: &Environment) -> RuntimeResult<()> {
    for expr in exprs.iter() {
        expand_and_resolve_everything_inner(expr, scope, env)?;
    }
    Ok(())
}

/// Try to expand a macro call, returning Some(expanded_expr_type) if it's a macro, None otherwise
pub fn try_expand_macro_call(
    call: &Call,
    resolved_var: Var,
    env: &Environment,
) -> RuntimeResult<Option<ExprType>> {
    let symbol = match resolved_var {
        Var::Global(symbol) => symbol,
        _ => return Ok(None), // Only global symbols can be macros
    };

    // Try to load as a macro using guard API
    let guard = match env.load_symbol_guard(symbol, Some(FuncCellType::Macro)) {
        Ok(g) => g,
        Err(_) => return Ok(None), // Symbol not found
    };
    let val = guard.as_ref();
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
        LispObject::Indirect(indirect) => {
            let obj = indirect.0.get().clone().untag();
            lisp_object_to_tokens(obj)
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
