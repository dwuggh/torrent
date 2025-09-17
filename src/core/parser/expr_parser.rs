use std::cell::{Cell, RefCell};
use std::rc::Rc;

use chumsky::extra::SimpleState;
use chumsky::input::{Stream, ValueInput};
use chumsky::inspector::Inspector;
use chumsky::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::core::env::Environment;
use crate::core::ident::Ident;
use crate::core::parser::expr::{
    new_refbox, Args, Expr, If, Let, Literal, Number, Progn, SpecialForm, Var,
};
use crate::core::parser::lexer::Token;

// type NodeInput<'s> = impl Input<'s, Node, SimpleSpan>;

struct FunctionFrame {
    args: Vec<Ident>,
    option_args: Vec<Ident>,
    rest_args: Option<Ident>,
    captures: FxHashSet<Ident>,
    id: u32,
}

type LexicalScope = Rc<Vec<(Ident, Option<Expr>)>>;

// struct LexicalScope {
//     binds: Vec<Ident>,
//     recursive: bool,
//     id: u32,
// }

enum Scope {
    Function(FunctionFrame),
    Lexical(LexicalScope),
    Global,
}

struct ScopeChain<'a> {
    scopes: Vec<Scope>,
    current: usize,
    prev: Option<usize>,
    env: &'a Environment,
}

impl<'a> ScopeChain<'a> {
    fn new(env: &'a Environment) -> Self {
        let mut scopes = Vec::new();
        scopes.push(Scope::Global);
        Self {
            scopes,
            current: 0,
            prev: None,
            env,
        }
    }

    fn resolve(&self, ident: Ident) {}
}

impl<'a, 'src, I> Inspector<'src, I> for ScopeChain<'a>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    type Checkpoint = usize;

    fn on_token(&mut self, token: &I::Token) {
        todo!()
    }

    fn on_save<'parse>(
        &self,
        cursor: &chumsky::input::Cursor<'src, 'parse, I>,
    ) -> Self::Checkpoint {
        self.current
    }

    fn on_rewind<'parse>(
        &mut self,
        marker: &chumsky::input::Checkpoint<'src, 'parse, I, Self::Checkpoint>,
    ) {
        // TODO pop self.current?
        self.current = *marker.inspector();
    }
}

type Error<'s> = extra::Err<Rich<'s, Token<'s>>>;
type Extra<'s> = extra::Full<Rich<'s, Token<'s>>, (), ()>;

fn parser<'s, I>() -> impl Parser<'s, I, Expr, Extra<'s>>
where
    I: ValueInput<'s, Token = Token<'s>, Span = SimpleSpan>,
{
    let id = 0;
    recursive(|expr| {
        // let binding = select! {
        //     Node::Sexp(nodes) if nodes.len() == 2 => {
        //         match (&nodes[0], &nodes[1]) {
        //             (Node::Ident(name), value_node) => {
        //                 // Convert value_node to Expr (you'll need to implement this)
        //                 let value = expr.clone();
        //                 (*name, value)
        //             }
        //             _ => return Err(/* error for invalid binding format */)
        //         }
        //     }
        // };

        let refexpr = expr.clone().map(RefCell::new).boxed();
        let vecref = refexpr.clone().repeated().collect::<Vec<_>>().boxed();
        let boxref = expr.clone().map(new_refbox);

        let ident = any::<I, Extra>().try_map(|node, span| match node {
            Token::Ident(ident) => Ok(ident),
            _ => Err(Rich::custom(span, "Expected identifier")),
        });
        let ident_interned = ident.map(|ident| Ident::from(ident));

        let keyword = |kw| just(Token::Ident(kw)).ignored();

        let nil = keyword("nil")
            .or(just(Token::LParen).ignore_then(just(Token::RParen).ignored()))
            .map(|_| Expr::Nil);

        let vector = vecref
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket));

        let progn = vecref.clone().map(Progn::new).boxed();

        let symbol = ident.map(|ident| Cell::new(Var::Symbol(ident.into())));

        let bindings = choice((
            ident_interned.then(refexpr.map(Option::Some)),
            ident_interned.map(|ident| (ident, None)),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .map(Rc::new)
        .boxed();

        let let_expr = sexp(keyword("let").ignore_then(bindings).then(progn.clone())).map(
            |(bindings, body)| Expr::SpecialForm(SpecialForm::Let(Let { bindings, body, id: 1 })),
        ).boxed();

        let if_expr = sexp(
            keyword("if")
                .ignore_then(boxref.clone())
                .then(boxref.clone())
                .then(progn.clone()),
        )
            .map(|((cond, then), els)| Expr::SpecialForm(SpecialForm::If(If { cond, then, els }))).boxed();

        let normal_arg = ident
            .try_map(|ident, span| {
                if ident.starts_with('&') {
                    Err(Rich::custom(span, "unexpected &"))
                } else {
                    Ok(Ident::from(ident))
                }
            })
            .boxed();
        let optional_arg = keyword("&optional")
            .ignore_then(normal_arg.clone())
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .boxed();

        let args = normal_arg
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
            })
            .boxed();

        let string = any::<I, Extra>()
            .try_map(|node, span| match node {
                Token::Str(str) => Ok(str),
                _ => Err(Rich::custom(span, "Expected string")),
            })
            .boxed();
        let interactive = keyword;
        // let lambda = keyword("lambda")
        //     .ignore_then()

        let primitive = select! {
            Token::Integer(n) => Expr::new_int(n),
            Token::Float(f) => Expr::new_float(f),
            Token::Character(c) => Expr::new_char(c),
            Token::Str(s) => Expr::new_str(s),
        };
        choice((primitive, let_expr, if_expr))
    })
}

fn sexp<'s, I, O, E>(exprs: impl Parser<'s, I, O, E>) -> impl Parser<'s, I, O, E>
where
    I: ValueInput<'s, Token = Token<'s>, Span = SimpleSpan>,
    E: extra::ParserExtra<'s, I>,
{
    exprs.delimited_by(just(Token::LParen), just(Token::RParen))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr() {
        // parser().
    }
}
