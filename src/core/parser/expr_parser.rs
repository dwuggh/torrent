use std::cell::{Cell, RefCell};
use std::rc::Rc;

use chumsky::input::ValueInput;
use chumsky::prelude::*;

use crate::core::ident::Ident;
use crate::core::number::*;
use crate::core::parser::token::Token;
use crate::core::parser::{expr::*, Span};
use crate::core::string::LispStr;

type Extra<'s> = extra::Full<Rich<'s, Token, Span>, (), ()>;

pub fn exprs_parser<'s, I>() -> impl Parser<'s, I, Vec<Expr>, Extra<'s>>
where
    I: ValueInput<'s, Token = Token, Span = Span>,
{
    expr_parser().repeated().collect::<Vec<_>>()
}

pub fn expr_parser<'s, I>() -> impl Parser<'s, I, Expr, Extra<'s>>
where
    I: ValueInput<'s, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let refexpr = expr.clone().boxed();
        let vecref = refexpr.clone().repeated().collect::<Vec<_>>().boxed();
        let boxref = expr.clone().map(new_box);

        let comments = just(Token::Comment).ignored().repeated();

        let ident = any::<I, Extra>().try_map(|node, span| match node {
            Token::Ident(ident) => Ok(ident),
            _ => Err(Rich::custom(span, "Expected identifier")),
        });
        let ident_interned = ident.map(|ident| Ident::from(ident));
        let arg = ident_interned.clone().map(Arg::new_uncap);
        let keyword = |kw: &str| just(Token::Ident(kw.into())).ignored();

        let nil = keyword("nil")
            .or(just(Token::LParen).ignore_then(just(Token::RParen).ignored()))
            .map_with(|_, ex| Expr::new(ExprType::Nil, ex.span()));

        let literal = select! {
            Token::Integer(n) => Literal::Number(Number::Integer(LispInteger::new(n))),
            Token::Float(f) => Literal::Number(Number::Real(LispFloat::new(f))),
            Token::Character(c) => Literal::Character(LispCharacter::new(c)),
            Token::Str(s) => Literal::String(LispStr::new(s))
        }
        .boxed();

        let literal_expr = literal
            .clone()
            .map_with(|lit, ex| Expr::new(ExprType::Literal(lit), ex.span()));

        let vector = vecref
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map_with(|vector, ex| Expr::new(ExprType::Vector(vector), ex.span()))
            .boxed();

        let progn = vecref.clone().map(Progn::new).boxed();

        let var = ident.map(|ident| Cell::new(Var::Unresolved(ident))).boxed();
        let symbol_expr = var
            .clone()
            .map_with(|var, ex| Expr::new(ExprType::Symbol(var), ex.span()))
            .boxed();
        let string = any::<I, Extra>()
            .try_map(|node, span| match node {
                Token::Str(str) => Ok(str),
                _ => Err(Rich::custom(span, "Expected string")),
            })
            .boxed();

        // call

        let call = sexp(var.clone().then(vecref.clone()))
            .map_with(|(symbol, args), ex| {
                Expr::new(ExprType::Call(Call { symbol, args }), ex.span())
            })
            .boxed();

        // special forms

        // let
        let bindings = sexp(
            choice((
                sexp(arg.then(refexpr.clone().map(Option::Some))).boxed(),
                arg.map(|ident| (ident, None)),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .map(Rc::new),
        )
        .boxed();

        let let_expr = sexp(
            keyword("let")
                .ignore_then(bindings.clone())
                .then(progn.clone()),
        )
        .map(|(bindings, body)| Let {
            bindings,
            body,
            id: 1,
        })
        .boxed();

        // let* TODO
        let let_star_expr = sexp(keyword("let*").ignore_then(bindings).then(progn.clone()))
            .map(|(bindings, body)| LetStar {
                bindings,
                body,
                id: 1,
            })
            .boxed();

        // if
        let if_expr = sexp(
            keyword("if")
                .ignore_then(boxref.clone())
                .then(boxref.clone())
                .then(progn.clone()),
        )
        .map(|((cond, then), els)| If { cond, then, els })
        .boxed();

        // lambda

        let normal_arg = ident
            .try_map(|ident, span| {
                if ident.text().starts_with('&') {
                    Err(Rich::custom(span, "unexpected &"))
                } else {
                    Ok(Ident::from(ident))
                }
            })
            .map(Arg::new_uncap)
            .boxed();
        let optional_arg = keyword("&optional")
            .ignore_then(normal_arg.clone())
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .boxed();

        let args = sexp(
            normal_arg
                .clone()
                .repeated()
                .collect::<Vec<_>>()
                .then(optional_arg.or_not())
                .then(keyword("&rest").ignore_then(normal_arg.clone()).or_not())
                .map(|((normal, optional), rest)| {
                    let args = Args {
                        normal,
                        optional,
                        rest,
                    };
                    Rc::new(args)
                }),
        )
        .boxed();

        let interactive = sexp(
            keyword("interactive")
                .ignore_then(string.clone().or_not())
                .then(
                    var.clone()
                        .repeated()
                        .at_least(1)
                        .collect::<Vec<_>>()
                        .or_not(),
                ),
        )
        .map(|(arg_desc, modes)| Interactive { arg_desc, modes })
        .boxed();

        let lambda = sexp(
            keyword("lambda")
                .ignore_then(args)
                .then(string.clone().or_not())
                .then(interactive.or_not())
                .then(progn.clone()),
        )
        .map(|(((args, docstring), interactive), body)| Lambda {
            args,
            docstring,
            interactive,
            declare: None, // TODO declare parsing
            body,
            captures: Rc::new(RefCell::new(Vec::new())),
        })
        .boxed();

        let quoted_data = recursive(|quoted_data| {
            let unquote = just(Token::Unquote).ignore_then(boxref.clone().map(QuotedData::Unquote));
            let unquote_splice = just(Token::UnquoteSplice)
                .ignore_then(boxref.clone().map(QuotedData::UnquoteSplice));

            let list = sexp(quoted_data.clone().repeated().collect::<Vec<_>>()).boxed();
            let vector = quoted_data
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBracket), just(Token::RBracket));
            choice((
                var.clone().map(QuotedData::Symbol),
                literal.clone().map(QuotedData::Literal),
                list.map(QuotedData::List),
                vector.map(QuotedData::Vector),
                unquote,
                unquote_splice,
            ))
        });

        let quote = choice((
            just(Token::Quote).map(|_| QuoteKind::Quote),
            just(Token::Backquote).map(|_| QuoteKind::Backquote),
        ))
        .then(quoted_data)
        .map(|(kind, expr)| Quote { kind, expr })
        .boxed();

        // and/or
        let and_expr = sexp(keyword("and").ignore_then(vecref.clone()))
            .map_with(|exprs, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::And(exprs)), ex.span())
            })
            .boxed();

        let or_expr = sexp(keyword("or").ignore_then(vecref.clone()))
            .map_with(|exprs, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Or(exprs)), ex.span())
            })
            .boxed();

        // catch
        let catch_expr = sexp(
            keyword("catch")
                .ignore_then(boxref.clone())
                .then(progn.clone()),
        )
        .map(|(tag, body)| Catch { tag, body })
        .boxed();

        // cond
        let cond_clause = sexp(refexpr.clone().then(progn.clone()))
            .map(|(condition, body)| CondClause { condition, body })
            .boxed();

        let cond_expr =
            sexp(keyword("cond").ignore_then(cond_clause.repeated().collect::<Vec<_>>()))
                .map(|clauses| Cond { clauses })
                .boxed();

        // condition-case
        let condition_handler = sexp(refexpr.clone().then(progn.clone()))
            .map(|(condition, body)| ConditionHandler { condition, body })
            .boxed();

        let condition_case_expr = sexp(
            keyword("condition-case")
                .ignore_then(ident_interned.or_not())
                .then(boxref.clone())
                .then(condition_handler.repeated().collect::<Vec<_>>()),
        )
        .boxed()
        .map(|((var, protected), handlers)| ConditionCase {
            var,
            protected,
            handlers,
        })
        .boxed();

        // defvar/defconst
        let defvar_expr = sexp(
            keyword("defvar")
                .ignore_then(ident_interned)
                .then(boxref.clone().or_not())
                .then(string.clone().or_not()),
        )
        .map(|((symbol, value), docstring)| Defvar {
            symbol,
            value,
            docstring,
        })
        .boxed();

        let defconst_expr = sexp(
            keyword("defconst")
                .ignore_then(ident_interned)
                .then(boxref.clone())
                .then(string.clone().or_not()),
        )
        .map(|((symbol, value), docstring)| Defconst {
            symbol,
            value,
            docstring,
        })
        .boxed();

        // prog1/prog2
        let prog1_expr = sexp(
            keyword("prog1")
                .ignore_then(boxref.clone())
                .then(progn.clone()),
        )
        .map(|(first, rest)| Prog1 { first, rest })
        .boxed();

        let prog2_expr = sexp(
            keyword("prog2")
                .ignore_then(boxref.clone())
                .then(boxref.clone())
                .then(progn.clone()),
        )
        .map(|((first, second), rest)| Prog2 {
            first,
            second,
            rest,
        })
        .boxed();

        // progn
        let progn_expr = sexp(keyword("progn").ignore_then(progn.clone()))
            .map_with(|body, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Progn(body)), ex.span())
            })
            .boxed();

        // set/setq/setq-default
        let set_expr = sexp(
            keyword("set")
                .ignore_then(ident_interned)
                .then(expr.clone().map(Box::new)),
        )
        .map(|(symbol, value)| Set {
            symbol: new_cellvar(symbol),
            value,
        })
        .boxed();

        let setq_assignment = ident_interned.map(new_cellvar).then(refexpr.clone());
        let setq_expr = sexp(
            keyword("setq").ignore_then(setq_assignment.clone().repeated().collect::<Vec<_>>()),
        )
        .map(|assignments| Setq { assignments })
        .boxed();

        let setq_default_expr = sexp(
            keyword("setq-default").ignore_then(setq_assignment.repeated().collect::<Vec<_>>()),
        )
        .map(|assignments| SetqDefault { assignments })
        .boxed();

        // save-* forms
        let save_current_buffer_expr =
            sexp(keyword("save-current-buffer").ignore_then(progn.clone()))
                .map_with(|body, ex| {
                    Expr::new(
                        ExprType::SpecialForm(SpecialForm::SaveCurrentBuffer(body)),
                        ex.span(),
                    )
                })
                .boxed();

        let save_excursion_expr = sexp(keyword("save-excursion").ignore_then(progn.clone()))
            .map_with(|body, ex| {
                Expr::new(
                    ExprType::SpecialForm(SpecialForm::SaveExcursion(body)),
                    ex.span(),
                )
            })
            .boxed();

        let save_restriction_expr = sexp(keyword("save-restriction").ignore_then(progn.clone()))
            .map_with(|body, ex| {
                Expr::new(
                    ExprType::SpecialForm(SpecialForm::SaveRestriction(body)),
                    ex.span(),
                )
            })
            .boxed();

        // unwind-protect
        let unwind_protect_expr = sexp(
            keyword("unwind-protect")
                .ignore_then(boxref.clone())
                .then(progn.clone()),
        )
        .map(|(protected, cleanup)| UnwindProtect { protected, cleanup })
        .boxed();

        // while
        let while_expr = sexp(
            keyword("while")
                .ignore_then(boxref.clone())
                .then(progn.clone()),
        )
        .map(|(condition, body)| While { condition, body })
        .boxed();

        let special_form = choice((
            lambda.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Lambda(form)), ex.span())
            }),
            if_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::If(form)), ex.span())
            }),
            let_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Let(form)), ex.span())
            }),
            let_star_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::LetStar(form)), ex.span())
            }),
            quote.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Quote(form)), ex.span())
            }),
            and_expr,
            or_expr,
            catch_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Catch(form)), ex.span())
            }),
            cond_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Cond(form)), ex.span())
            }),
            condition_case_expr.map_with(|form, ex| {
                Expr::new(
                    ExprType::SpecialForm(SpecialForm::ConditionCase(form)),
                    ex.span(),
                )
            }),
            defvar_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Defvar(form)), ex.span())
            }),
            defconst_expr.map_with(|form, ex| {
                Expr::new(
                    ExprType::SpecialForm(SpecialForm::Defconst(form)),
                    ex.span(),
                )
            }),
            prog1_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Prog1(form)), ex.span())
            }),
            prog2_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Prog2(form)), ex.span())
            }),
            progn_expr,
            set_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Set(form)), ex.span())
            }),
            setq_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::Setq(form)), ex.span())
            }),
            setq_default_expr.map_with(|form, ex| {
                Expr::new(
                    ExprType::SpecialForm(SpecialForm::SetqDefault(form)),
                    ex.span(),
                )
            }),
            save_current_buffer_expr,
            save_excursion_expr,
            save_restriction_expr,
            unwind_protect_expr.map_with(|form, ex| {
                Expr::new(
                    ExprType::SpecialForm(SpecialForm::UnwindProtect(form)),
                    ex.span(),
                )
            }),
            while_expr.map_with(|form, ex| {
                Expr::new(ExprType::SpecialForm(SpecialForm::While(form)), ex.span())
            }),
        ));

        choice((nil, literal_expr, symbol_expr, vector, special_form, call)).padded_by(comments)
    })
}

fn sexp<'s, I, O, E>(exprs: impl Parser<'s, I, O, E>) -> impl Parser<'s, I, O, E>
where
    I: ValueInput<'s, Token = Token, Span = Span>,
    E: extra::ParserExtra<'s, I>,
{
    exprs.delimited_by(just(Token::LParen), just(Token::RParen))
}

#[cfg(test)]
mod tests {
    use crate::core::number::LispInteger;

    use super::*;
    use chumsky::input::Stream;
    use chumsky::Parser;
    use logos::Logos;

    fn parse_src<'s>(src: &'s str) -> Result<Expr, Vec<Rich<'s, Token, Span>>> {
        let tokens = Token::lexer(src).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(_) => (Token::Error, span.into()),
        });
        let stream = Stream::from_iter(tokens).map((0..src.len()).into(), |(t, s)| (t, s));
        expr_parser().parse(stream).into_result()
    }

    #[test]
    fn test_parse_primitives() {
        // Test integer
        let result = parse_src("42").unwrap();
        match &*result.ty.borrow() {
            ExprType::Literal(Literal::Number(Number::Integer(n))) => assert_eq!(n.0, 42),
            _ => panic!("Expected integer literal"),
        }

        // Test float
        let result = parse_src("3.14").unwrap();
        match &*result.ty.borrow() {
            ExprType::Literal(Literal::Number(Number::Real(f))) => assert_eq!(f.0, 3.14),
            _ => panic!("Expected float literal"),
        }

        // Test character
        let result = parse_src("?a").unwrap();
        match &*result.ty.borrow() {
            ExprType::Literal(Literal::Character(c)) => assert_eq!(c.0.to_char().unwrap(), 'a'),
            _ => panic!("Expected character literal"),
        }

        // Test string
        let result = parse_src("\"hello\"").unwrap();
        match &*result.ty.borrow() {
            ExprType::Literal(Literal::String(s)) => assert_eq!(s.to_string(), "hello"),
            _ => panic!("Expected string literal"),
        }

        // Test nil (keyword)
        let result = parse_src("nil").unwrap();
        match &*result.ty.borrow() {
            ExprType::Nil => {}
            _ => panic!("Expected nil"),
        }

        // Test nil (empty list)
        let result = parse_src("()").unwrap();
        let ty = result.ty();
        match &*ty {
            ExprType::Nil => {}
            _ => panic!("Expected nil from empty list"),
        }
    }

    #[test]
    fn test_parse_if_expression() {
        // Test (if condition then else)
        let result = parse_src("(if 1 2 3)").unwrap();
        let expr = result.ty();

        match &*expr {
            ExprType::SpecialForm(SpecialForm::If(if_expr)) => {
                // Verify condition is integer 1
                match &*if_expr.cond.ty.borrow() {
                    ExprType::Literal(Literal::Number(_)) => {}
                    _ => panic!("Expected condition to be integer 1"),
                }

                // Verify then is integer 2
                match *if_expr.then.ty.borrow() {
                    ExprType::Literal(Literal::Number(Number::Integer(LispInteger(2)))) => {}
                    _ => panic!("Expected then to be integer 2"),
                }

                // Verify else body contains integer 3
                assert_eq!(if_expr.els.body.len(), 1);
                match &*if_expr.els.body[0].ty.borrow() {
                    ExprType::Literal(Literal::Number(Number::Integer(LispInteger(3)))) => {}
                    _ => panic!("Expected else to be integer 3"),
                }
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_parse_let_expression() {
        let result = parse_src("(let ((x 42)) x)").unwrap();
        let expr = result.ty();

        match &*expr {
            ExprType::SpecialForm(SpecialForm::Let(let_expr)) => {
                // Verify bindings
                assert_eq!(let_expr.bindings.len(), 1);
                let (arg, value) = &let_expr.bindings[0];
                assert_eq!(arg.ident.text(), "x");

                match &*value.as_ref().unwrap().ty() {
                    ExprType::Literal(Literal::Number(Number::Integer(LispInteger(42)))) => {}
                    _ => panic!("Expected binding value to be integer 42"),
                }

                // Verify body contains variable reference
                assert_eq!(let_expr.body.body.len(), 1);
                match &*let_expr.body.body[0].ty() {
                    ExprType::Symbol(var) => match var.get() {
                        Var::Unresolved(sym) => assert_eq!(sym.text(), "x"),
                        _ => panic!("Expected symbol variable"),
                    },
                    _ => panic!("Expected variable in let body"),
                }
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_parse_let_without_value() {
        let result = parse_src("(let (x) x)").unwrap();
        let expr = result.ty();

        match *expr {
            ExprType::SpecialForm(SpecialForm::Let(ref let_expr)) => {
                // Verify bindings
                assert_eq!(let_expr.bindings.len(), 1);
                let (arg, value) = &let_expr.bindings[0];
                assert_eq!(arg.ident.text(), "x");
                assert!(value.is_none(), "Expected no initial value");
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_parse_nested_expressions() {
        let result = parse_src("(if (let ((x 1)) x) 2 3)").unwrap();
        let expr = result.ty();

        match &*expr {
            ExprType::SpecialForm(SpecialForm::If(if_expr)) => {
                // Verify condition is a let expression
                match &*if_expr.cond.ty() {
                    ExprType::SpecialForm(SpecialForm::Let(_)) => {}
                    _ => panic!("Expected condition to be let expression"),
                }
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_parse_lambda() {
        let result = parse_src(
            "(lambda (x y &optional z &optional x &rest args)
(interactive \"P\")
)",
        )
        .unwrap();
        let expr = result.ty();

        match &*expr {
            ExprType::SpecialForm(SpecialForm::Lambda(lambda)) => {
                let normal = lambda.args.normal.len();
                let optional = lambda.args.optional.as_ref().unwrap();
                let rest = lambda.args.rest.clone();
                assert_eq!(normal, 2);
                assert_eq!(optional.len(), 2);
                assert!(rest.is_some());
                assert!(lambda.interactive.is_some());
            }
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_and_or() {
        let result = parse_src("(and 1 2 3)").unwrap();
        let expr = result.ty();
        match &*expr {
            ExprType::SpecialForm(SpecialForm::And(exprs)) => {
                assert_eq!(exprs.len(), 3);
            }
            _ => panic!("Expected and expression"),
        }

        let result = parse_src("(or nil t)").unwrap();
        let expr = result.ty();
        match &*expr {
            ExprType::SpecialForm(SpecialForm::Or(exprs)) => {
                assert_eq!(exprs.len(), 2);
            }
            _ => panic!("Expected or expression"),
        }
    }

    #[test]
    fn test_parse_cond() {
        let result = parse_src("(cond ((= x 1) 'one) ((= x 2) 'two) (t 'other))").unwrap();
        let expr = result.ty();
        match &*expr {
            ExprType::SpecialForm(SpecialForm::Cond(cond_expr)) => {
                assert_eq!(cond_expr.clauses.len(), 3);
            }
            _ => panic!("Expected cond expression"),
        }
    }

    #[test]
    fn test_parse_while() {
        let result = parse_src("(while (< i 10) (setq i (+ i 1)))").unwrap();
        let expr = result.ty();
        match &*expr {
            ExprType::SpecialForm(SpecialForm::While(_)) => {}
            _ => panic!("Expected while expression"),
        }
    }
}
