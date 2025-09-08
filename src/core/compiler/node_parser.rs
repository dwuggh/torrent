use chumsky::prelude::*;
use std::sync::Arc;

use crate::{
    ast::Node,
    core::{compiler::ir::*, ident::Ident},
};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Option<(usize, usize)>,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}

pub fn node_to_expr() -> impl Parser<Node, Expr, Error = ParseError> + Clone {
    recursive(|expr| {
        // Atomic expressions
        let atom = select! {
            Node::Ident(ident) => Expr::Symbol(ident),
            Node::Integer(n) => Expr::Literal(Literal::Number(Number::FixedInteger(n))),
            Node::Float(f) => Expr::Literal(Literal::Number(Number::Real(f))),
            Node::Char(c) => Expr::Literal(Literal::Character(c)),
            Node::Str(s) => Expr::Literal(Literal::String(s)),
            Node::Nil => Expr::Nil,
        };

        // Vector expressions
        let vector = select! {
            Node::Vector(nodes) => nodes,
        }
        .then(expr.clone().repeated().collect::<Vec<_>>())
        .map(|(_, exprs)| Expr::Vector(exprs));

        // Special forms and function calls
        let sexp = select! {
            Node::Sexp(nodes) => nodes,
        }
        .then(
            choice((
                // Special forms
                if_form(expr.clone()),
                lambda_form(expr.clone()),
                let_form(expr.clone()),
                let_star_form(expr.clone()),
                defvar_form(expr.clone()),
                defconst_form(expr.clone()),
                set_form(expr.clone()),
                setq_form(expr.clone()),
                setq_default_form(expr.clone()),
                and_form(expr.clone()),
                or_form(expr.clone()),
                progn_form(expr.clone()),
                prog1_form(expr.clone()),
                prog2_form(expr.clone()),
                quote_form(expr.clone()),
                function_form(expr.clone()),
                cond_form(expr.clone()),
                while_form(expr.clone()),
                catch_form(expr.clone()),
                unwind_protect_form(expr.clone()),
                condition_case_form(expr.clone()),
                save_current_buffer_form(expr.clone()),
                save_excursion_form(expr.clone()),
                save_restriction_form(expr.clone()),
                interactive_form(expr.clone()),
                // Function call (fallback)
                function_call(expr.clone()),
            ))
        )
        .map(|(_, expr)| expr);

        choice((atom, vector, sexp))
    })
}

fn if_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "if")
    })
    .then(
        expr.clone()
            .then(expr.clone())
            .then(expr.clone().or_not())
            .map(|((cond, then_branch), else_branch)| {
                Expr::SpecialForm(SpecialForm::If(If {
                    cond: Arc::new(cond),
                    then: Arc::new(then_branch),
                    els: else_branch.map_or(vec![], |e| vec![e]),
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn lambda_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "lambda")
    })
    .then(
        lambda_args()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(args, body)| {
                Expr::SpecialForm(SpecialForm::Lambda(Lambda {
                    args,
                    docstring: None,
                    interactive: None,
                    declare: None,
                    body,
                    captures: vec![],
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn lambda_args() -> impl Parser<Node, Vec<Arg>, Error = ParseError> + Clone {
    select! {
        Node::Nil => vec![],
        Node::Sexp(nodes) => {
            let mut args = Vec::new();
            let mut arg_type = ArgsType::Normal;
            
            for node in nodes {
                match node {
                    Node::Ident(ident) => {
                        let name = ident.text();
                        match name {
                            "&optional" => {
                                arg_type = ArgsType::Optional;
                                continue;
                            }
                            "&rest" => {
                                arg_type = ArgsType::Rest;
                                continue;
                            }
                            _ => {
                                args.push(Arg {
                                    ty: arg_type,
                                    ident: *ident,
                                });
                            }
                        }
                    }
                    _ => return Err(ParseError {
                        message: "Invalid lambda argument".to_string(),
                        span: None,
                    }),
                }
            }
            Ok(args)
        }
    }
    .try_map(|result, _span| result)
}

fn let_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "let")
    })
    .then(
        let_bindings()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(bindings, body)| {
                Expr::SpecialForm(SpecialForm::Let(Let { bindings, body }))
            })
    )
    .map(|(_, expr)| expr)
}

fn let_star_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "let*")
    })
    .then(
        let_bindings()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(bindings, body)| {
                Expr::SpecialForm(SpecialForm::LetStar(LetStar { bindings, body }))
            })
    )
    .map(|(_, expr)| expr)
}

fn let_bindings() -> impl Parser<Node, Vec<(Ident, Option<Expr>)>, Error = ParseError> + Clone {
    select! {
        Node::Nil => vec![],
        Node::Sexp(nodes) => {
            let mut bindings = Vec::new();
            for binding_node in nodes {
                match binding_node {
                    Node::Ident(ident) => {
                        bindings.push((*ident, None));
                    }
                    Node::Sexp(binding_parts) => {
                        if binding_parts.len() != 2 {
                            return Err(ParseError {
                                message: "Invalid let binding".to_string(),
                                span: None,
                            });
                        }
                        if let Node::Ident(ident) = &binding_parts[0] {
                            // We need to parse the value expression recursively
                            // For now, we'll create a placeholder
                            bindings.push((*ident, None)); // TODO: parse value
                        } else {
                            return Err(ParseError {
                                message: "Let binding must start with identifier".to_string(),
                                span: None,
                            });
                        }
                    }
                    _ => return Err(ParseError {
                        message: "Invalid let binding format".to_string(),
                        span: None,
                    }),
                }
            }
            Ok(bindings)
        }
    }
    .try_map(|result, _span| result)
}

fn defvar_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "defvar")
    })
    .then(
        select! { Node::Ident(ident) => ident }
            .then(expr.clone().or_not())
            .then(select! { Node::Str(s) => s }.or_not())
            .map(|((symbol, value), docstring)| {
                Expr::SpecialForm(SpecialForm::Defvar(Defvar {
                    symbol,
                    value: value.map(Box::new),
                    docstring,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn defconst_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "defconst")
    })
    .then(
        select! { Node::Ident(ident) => ident }
            .then(expr.clone())
            .then(select! { Node::Str(s) => s }.or_not())
            .map(|((symbol, value), docstring)| {
                Expr::SpecialForm(SpecialForm::Defconst(Defconst {
                    symbol,
                    value: Box::new(value),
                    docstring,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn set_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "set")
    })
    .then(
        select! { Node::Ident(ident) => ident }
            .then(expr.clone())
            .map(|(symbol, value)| {
                Expr::SpecialForm(SpecialForm::Set(Set {
                    symbol,
                    value: Box::new(value),
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn setq_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "setq")
    })
    .then(
        setq_assignments(expr.clone())
            .map(|assignments| {
                Expr::SpecialForm(SpecialForm::Setq(Setq { assignments }))
            })
    )
    .map(|(_, expr)| expr)
}

fn setq_default_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "setq-default")
    })
    .then(
        setq_assignments(expr.clone())
            .map(|assignments| {
                Expr::SpecialForm(SpecialForm::SetqDefault(SetqDefault { assignments }))
            })
    )
    .map(|(_, expr)| expr)
}

fn setq_assignments(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Vec<(Ident, Expr)>, Error = ParseError> + Clone {
    select! {
        nodes if nodes.len() % 2 == 1 => { // Skip the first element (setq/setq-default)
            let pairs = &nodes[1..];
            let mut assignments = Vec::new();
            for chunk in pairs.chunks(2) {
                if let [Node::Ident(ident), value_node] = chunk {
                    // TODO: parse value_node to Expr
                    assignments.push((*ident, Expr::Nil)); // placeholder
                }
            }
            assignments
        }
    }
}

fn and_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "and")
    })
    .then(
        expr.clone().repeated().collect::<Vec<_>>()
            .map(|exprs| Expr::SpecialForm(SpecialForm::And(exprs)))
    )
    .map(|(_, expr)| expr)
}

fn or_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "or")
    })
    .then(
        expr.clone().repeated().collect::<Vec<_>>()
            .map(|exprs| Expr::SpecialForm(SpecialForm::Or(exprs)))
    )
    .map(|(_, expr)| expr)
}

fn progn_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "progn")
    })
    .then(
        expr.clone().repeated().collect::<Vec<_>>()
            .map(|exprs| Expr::SpecialForm(SpecialForm::Progn(exprs)))
    )
    .map(|(_, expr)| expr)
}

fn prog1_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "prog1")
    })
    .then(
        expr.clone()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(first, rest)| {
                Expr::SpecialForm(SpecialForm::Prog1(Prog1 {
                    first: Box::new(first),
                    rest,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn prog2_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "prog2")
    })
    .then(
        expr.clone()
            .then(expr.clone())
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|((first, second), rest)| {
                Expr::SpecialForm(SpecialForm::Prog2(Prog2 {
                    first: Box::new(first),
                    second: Box::new(second),
                    rest,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn quote_form(_expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "quote")
    })
    .then(
        any().map(|node| {
            let quoted_data = node_to_quoted_data(&node, false);
            Expr::SpecialForm(SpecialForm::Quote(Quote {
                kind: QuoteKind::Quote,
                expr: quoted_data,
            }))
        })
    )
    .map(|(_, expr)| expr)
}

fn function_form(_expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "function")
    })
    .then(
        select! { Node::Ident(ident) => ident }
            .map(|name| {
                Expr::SpecialForm(SpecialForm::Function(Function { name }))
            })
    )
    .map(|(_, expr)| expr)
}

fn cond_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "cond")
    })
    .then(
        cond_clauses(expr.clone())
            .map(|clauses| Expr::SpecialForm(SpecialForm::Cond(Cond { clauses })))
    )
    .map(|(_, expr)| expr)
}

fn cond_clauses(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Vec<CondClause>, Error = ParseError> + Clone {
    select! {
        nodes => {
            let mut clauses = Vec::new();
            for clause_node in &nodes[1..] { // Skip the 'cond' symbol
                if let Node::Sexp(clause) = clause_node {
                    if !clause.is_empty() {
                        // TODO: parse condition and body expressions
                        clauses.push(CondClause {
                            condition: Expr::Nil, // placeholder
                            body: vec![],
                        });
                    }
                }
            }
            clauses
        }
    }
}

fn while_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "while")
    })
    .then(
        expr.clone()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(condition, body)| {
                Expr::SpecialForm(SpecialForm::While(While {
                    condition: Box::new(condition),
                    body,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn catch_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "catch")
    })
    .then(
        expr.clone()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(tag, body)| {
                Expr::SpecialForm(SpecialForm::Catch(Catch {
                    tag: Box::new(tag),
                    body,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn unwind_protect_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "unwind-protect")
    })
    .then(
        expr.clone()
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .map(|(protected, cleanup)| {
                Expr::SpecialForm(SpecialForm::UnwindProtect(UnwindProtect {
                    protected: Box::new(protected),
                    cleanup,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn condition_case_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "condition-case")
    })
    .then(
        condition_case_var()
            .then(expr.clone())
            .then(condition_handlers(expr.clone()))
            .map(|((var, protected), handlers)| {
                Expr::SpecialForm(SpecialForm::ConditionCase(ConditionCase {
                    var,
                    protected: Box::new(protected),
                    handlers,
                }))
            })
    )
    .map(|(_, expr)| expr)
}

fn condition_case_var() -> impl Parser<Node, Option<Ident>, Error = ParseError> + Clone {
    select! {
        Node::Ident(ident) => Some(ident),
        Node::Nil => None,
    }
}

fn condition_handlers(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Vec<ConditionHandler>, Error = ParseError> + Clone {
    select! {
        nodes => {
            let mut handlers = Vec::new();
            for handler_node in nodes {
                if let Node::Sexp(_handler) = handler_node {
                    // TODO: parse handler condition and body
                    handlers.push(ConditionHandler {
                        condition: Expr::Nil, // placeholder
                        body: vec![],
                    });
                }
            }
            handlers
        }
    }
}

fn save_current_buffer_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "save-current-buffer")
    })
    .then(
        expr.clone().repeated().collect::<Vec<_>>()
            .map(|body| Expr::SpecialForm(SpecialForm::SaveCurrentBuffer(SaveCurrentBuffer { body })))
    )
    .map(|(_, expr)| expr)
}

fn save_excursion_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "save-excursion")
    })
    .then(
        expr.clone().repeated().collect::<Vec<_>>()
            .map(|body| Expr::SpecialForm(SpecialForm::SaveExcursion(SaveExcursion { body })))
    )
    .map(|(_, expr)| expr)
}

fn save_restriction_form(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "save-restriction")
    })
    .then(
        expr.clone().repeated().collect::<Vec<_>>()
            .map(|body| Expr::SpecialForm(SpecialForm::SaveRestriction(SaveRestriction { body })))
    )
    .map(|(_, expr)| expr)
}

fn interactive_form(_expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    filter(|nodes: &Vec<Node>| {
        matches!(nodes.first(), Some(Node::Ident(ident)) if ident.text() == "interactive")
    })
    .then(
        interactive_spec()
            .map(|interactive| Expr::SpecialForm(SpecialForm::Interactive(interactive)))
    )
    .map(|(_, expr)| expr)
}

fn interactive_spec() -> impl Parser<Vec<Node>, Interactive, Error = ParseError> + Clone {
    select! {
        nodes => {
            let arg_desc = if let Some(Node::Str(s)) = nodes.get(1) {
                Some(s.clone())
            } else {
                None
            };
            
            let mut modes = Vec::new();
            for arg in nodes.iter().skip(if arg_desc.is_some() { 2 } else { 1 }) {
                if let Node::Ident(ident) = arg {
                    modes.push(*ident);
                }
            }
            
            Interactive { arg_desc, modes }
        }
    }
}

fn function_call(expr: impl Parser<Node, Expr, Error = ParseError> + Clone) -> impl Parser<Vec<Node>, Expr, Error = ParseError> + Clone {
    expr.clone()
        .then(expr.clone().repeated().collect::<Vec<_>>())
        .map(|(func, args)| {
            Expr::Call(Call {
                func: Box::new(func),
                args,
            })
        })
}

fn node_to_quoted_data(node: &Node, _allow_unquote: bool) -> QuotedData {
    match node {
        Node::Ident(ident) => QuotedData::Symbol(*ident),
        Node::Integer(n) => QuotedData::Literal(Literal::Number(Number::FixedInteger(*n))),
        Node::Float(f) => QuotedData::Literal(Literal::Number(Number::Real(*f))),
        Node::Char(c) => QuotedData::Literal(Literal::Character(*c)),
        Node::Str(s) => QuotedData::Literal(Literal::String(s.clone())),
        Node::Nil => QuotedData::Symbol(Ident::from_string("nil")),
        Node::Vector(nodes) => {
            let items = nodes.iter().map(|n| node_to_quoted_data(n, false)).collect();
            QuotedData::Vector(items)
        }
        Node::Sexp(nodes) => {
            let items = nodes.iter().map(|n| node_to_quoted_data(n, false)).collect();
            QuotedData::List(items)
        }
        Node::Unquote | Node::UnquoteSplice | Node::Backquote => {
            // These should be handled in context
            QuotedData::Symbol(Ident::from_string("nil"))
        }
    }
}

pub fn parse_node_to_expr(node: Node) -> Result<Expr, ParseError> {
    node_to_expr().parse(node).into_result().map_err(|errs| {
        errs.into_iter().next().unwrap_or(ParseError {
            message: "Unknown parse error".to_string(),
            span: None,
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_atom() {
        let node = Node::Integer(42);
        let expr = parse_node_to_expr(node).unwrap();
        assert!(matches!(
            expr,
            Expr::Literal(Literal::Number(Number::FixedInteger(42)))
        ));
    }

    #[test]
    fn test_parse_symbol() {
        let node = Node::Ident(Ident::from_string("foo"));
        let expr = parse_node_to_expr(node).unwrap();
        assert!(matches!(expr, Expr::Symbol(_)));
    }

    #[test]
    fn test_parse_if() {
        let node = Node::Sexp(vec![
            Node::Ident(Ident::from_string("if")),
            Node::Ident(Ident::from_string("condition")),
            Node::Integer(1),
            Node::Integer(2),
        ]);
        let expr = parse_node_to_expr(node).unwrap();
        assert!(matches!(expr, Expr::SpecialForm(SpecialForm::If(_))));
    }

    #[test]
    fn test_parse_function_call() {
        let node = Node::Sexp(vec![
            Node::Ident(Ident::from_string("foo")),
            Node::Integer(1),
            Node::Integer(2),
        ]);
        let expr = parse_node_to_expr(node).unwrap();
        assert!(matches!(expr, Expr::Call(_)));
    }
}
