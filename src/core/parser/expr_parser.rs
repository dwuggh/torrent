use chumsky::extra::SimpleState;
use chumsky::input::{Stream, ValueInput};
use chumsky::inspector::Inspector;
use chumsky::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::core::env::Environment;
use crate::core::ident::Ident;
use crate::core::parser::expr::{Expr, Literal, Number};
use crate::core::parser::node::Node;

// type NodeInput<'s> = impl Input<'s, Node, SimpleSpan>;

struct FunctionFrame {
    args: Vec<Ident>,
    option_args: Vec<Ident>,
    rest_args: Option<Ident>,
    captures: FxHashSet<Ident>,
}

struct LexicalScope {
    binds: Vec<Ident>,
    recursive: bool,
}

enum Scope {
    Function(FunctionFrame),
    Lexical(LexicalScope),
    Global,
}

struct ScopeChain<'a> {
    scopes: Vec<Scope>,
    current: usize,
    prev: Option<usize>,
    env: &'a Environment
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
}

impl<'a, 'src, I> Inspector<'src, I> for ScopeChain<'a>
where
    I: ValueInput<'src, Token = Node, Span = SimpleSpan>,
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

type Error<'s> = extra::Err<Rich<'s, Node>>;
// type Extra<'s> = extra::Full<Rich<'s, Node>, SimpleState<>>;

fn parser<'s, I>() -> impl Parser<'s, I, Expr, extra::Err<Rich<'s, Node>>>
where
    I: ValueInput<'s, Token = Node, Span = SimpleSpan>,
{
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

        let ident = any::<I, Error>().try_map(|node, span| match node {
            Node::Ident(ident) => Ok(Expr::Symbol(ident)),
            _ => Err(Rich::custom(span, "Expected identifier")),
        });

        let sexp = any::<I, Error>().try_map(|node, span| match node {
            Node::Sexp(sexp) => Ok(sexp),
            _ => Err(Rich::custom(span, "Expected identifier")),
        });

        //  let bindings
        let bind = choice((ident.clone().then(expr.clone()), ident.clone()));
        let binds = bind.clone().repeated();

        let primitive = select! {
            Node::Integer(n) => Expr::new_int(n),
            Node::Float(f) => Expr::new_float(f),
            Node::Char(c) => Expr::new_char(c),
            Node::Str(s) => Expr::new_str(s),
        };
        choice((primitive,))
    })
}
