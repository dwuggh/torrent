use chumsky::prelude::*;
use std::sync::Arc;

use crate::{
    ast::{Node, NodeInput},
    core::{compiler::ir::*, ident::Ident},
};


type ParseError<'s> = extra::Err<Rich<'s, Node>>;


pub fn node_to_expr<'s>() -> impl Parser<'s, NodeInput, Expr, ParseError> + Clone {
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

        // Vector expressions - parse the contained nodes recursively
        let vector = select! {
            Node::Vector(nodes) => nodes,
        }
        .try_map(|nodes, span| {
            let mut exprs = Vec::new();
            for node in nodes {
                let node_input = NodeInput::new(vec![node]);
                match expr.clone().parse(node_input).into_result() {
                    Ok(parsed_expr) => exprs.push(parsed_expr),
                    Err(_) => return Err(ParseError {
                        message: "Failed to parse vector element".to_string(),
                        span: Some((span.start, span.end)),
                    }),
                }
            }
            Ok(Expr::Vector(exprs))
        });

        // Special forms and function calls
        let sexp = select! {
            Node::Sexp(nodes) => nodes,
        }
        .try_map(|nodes, span| {
            if nodes.is_empty() {
                return Ok(Expr::Nil);
            }

            // Check if this is a special form
            if let Some(Node::Ident(ident)) = nodes.first() {
                let name = ident.text();
                match name {
                    "if" => parse_if_form(&nodes, expr.clone(), span),
                    "lambda" => parse_lambda_form(&nodes, expr.clone(), span),
                    "let" => parse_let_form(&nodes, expr.clone(), span),
                    "let*" => parse_let_star_form(&nodes, expr.clone(), span),
                    "defvar" => parse_defvar_form(&nodes, expr.clone(), span),
                    "defconst" => parse_defconst_form(&nodes, expr.clone(), span),
                    "set" => parse_set_form(&nodes, expr.clone(), span),
                    "setq" => parse_setq_form(&nodes, expr.clone(), span),
                    "setq-default" => parse_setq_default_form(&nodes, expr.clone(), span),
                    "and" => parse_and_form(&nodes, expr.clone(), span),
                    "or" => parse_or_form(&nodes, expr.clone(), span),
                    "progn" => parse_progn_form(&nodes, expr.clone(), span),
                    "prog1" => parse_prog1_form(&nodes, expr.clone(), span),
                    "prog2" => parse_prog2_form(&nodes, expr.clone(), span),
                    "quote" => parse_quote_form(&nodes, span),
                    "function" => parse_function_form(&nodes, span),
                    "cond" => parse_cond_form(&nodes, expr.clone(), span),
                    "while" => parse_while_form(&nodes, expr.clone(), span),
                    "catch" => parse_catch_form(&nodes, expr.clone(), span),
                    "unwind-protect" => parse_unwind_protect_form(&nodes, expr.clone(), span),
                    "condition-case" => parse_condition_case_form(&nodes, expr.clone(), span),
                    "save-current-buffer" => parse_save_current_buffer_form(&nodes, expr.clone(), span),
                    "save-excursion" => parse_save_excursion_form(&nodes, expr.clone(), span),
                    "save-restriction" => parse_save_restriction_form(&nodes, expr.clone(), span),
                    "interactive" => parse_interactive_form(&nodes, span),
                    _ => parse_function_call(&nodes, expr.clone(), span),
                }
            } else {
                parse_function_call(&nodes, expr.clone(), span)
            }
        });

        choice((atom, vector, sexp))
    })
}

fn parse_if_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() < 3 || nodes.len() > 4 {
        return Err(ParseError {
            message: "if requires 2 or 3 arguments".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let cond = parse_single_expr(&nodes[1], expr_parser.clone())?;
    let then_branch = parse_single_expr(&nodes[2], expr_parser.clone())?;
    let else_branch = if nodes.len() == 4 {
        vec![parse_single_expr(&nodes[3], expr_parser)?]
    } else {
        vec![]
    };

    Ok(Expr::SpecialForm(SpecialForm::If(If {
        cond: Arc::new(cond),
        then: Arc::new(then_branch),
        els: else_branch,
    })))
}

fn parse_lambda_form(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() < 3 {
        return Err(ParseError {
            message: "lambda requires at least 2 arguments".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let args = parse_lambda_args(&nodes[1])?;
    let mut body = Vec::new();
    for node in &nodes[2..] {
        body.push(parse_single_expr(node, expr_parser.clone())?);
    }

    Ok(Expr::SpecialForm(SpecialForm::Lambda(Lambda {
        args,
        docstring: None,
        interactive: None,
        declare: None,
        body,
        captures: vec![],
    })))
}

fn parse_lambda_args(node: &Node) -> Result<Vec<Arg>, ParseError> {
    match node {
        Node::Nil => Ok(vec![]),
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
        _ => Err(ParseError {
            message: "Lambda args must be a list or nil".to_string(),
            span: None,
        }),
    }
}

fn parse_let_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() < 3 {
        return Err(ParseError {
            message: "let requires at least 2 arguments".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let bindings = parse_let_bindings(&nodes[1], expr_parser.clone())?;
    let mut body = Vec::new();
    for node in &nodes[2..] {
        body.push(parse_single_expr(node, expr_parser.clone())?);
    }

    Ok(Expr::SpecialForm(SpecialForm::Let(Let { bindings, body })))
}

fn parse_let_star_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() < 3 {
        return Err(ParseError {
            message: "let* requires at least 2 arguments".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let bindings = parse_let_bindings(&nodes[1], expr_parser.clone())?;
    let mut body = Vec::new();
    for node in &nodes[2..] {
        body.push(parse_single_expr(node, expr_parser.clone())?);
    }

    Ok(Expr::SpecialForm(SpecialForm::LetStar(LetStar { bindings, body })))
}

fn parse_let_bindings<'s>(
    node: &Node,
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
) -> Result<Vec<(Ident, Option<Expr>)>, ParseError> {
    match node {
        Node::Nil => Ok(vec![]),
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
                            let value = parse_single_expr(&binding_parts[1], expr_parser.clone())?;
                            bindings.push((*ident, Some(value)));
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
        _ => Err(ParseError {
            message: "Let bindings must be a list or nil".to_string(),
            span: None,
        }),
    }
}

fn parse_defvar_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() < 2 || nodes.len() > 4 {
        return Err(ParseError {
            message: "defvar requires 1-3 arguments".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let symbol = match &nodes[1] {
        Node::Ident(ident) => *ident,
        _ => return Err(ParseError {
            message: "defvar first argument must be a symbol".to_string(),
            span: Some((span.start, span.end)),
        }),
    };

    let value = if nodes.len() > 2 {
        Some(Box::new(parse_single_expr(&nodes[2], expr_parser)?))
    } else {
        None
    };

    let docstring = if nodes.len() > 3 {
        match &nodes[3] {
            Node::Str(s) => Some(s.clone()),
            _ => return Err(ParseError {
                message: "defvar docstring must be a string".to_string(),
                span: Some((span.start, span.end)),
            }),
        }
    } else {
        None
    };

    Ok(Expr::SpecialForm(SpecialForm::Defvar(Defvar {
        symbol,
        value,
        docstring,
    })))
}


fn parse_function_call<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    _span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.is_empty() {
        return Err(ParseError {
            message: "Empty function call".to_string(),
            span: None,
        });
    }

    let func = parse_single_expr(&nodes[0], expr_parser.clone())?;
    let mut args = Vec::new();
    for node in &nodes[1..] {
        args.push(parse_single_expr(node, expr_parser.clone())?);
    }

    Ok(Expr::Call(Call {
        func: Box::new(func),
        args,
    }))
}

// Placeholder implementations for remaining forms
fn parse_defconst_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "defconst not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_set_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "set not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_setq_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "setq not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_setq_default_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "setq-default not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_and_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    _span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    let mut exprs = Vec::new();
    for node in &nodes[1..] {
        exprs.push(parse_single_expr(node, expr_parser.clone())?);
    }
    Ok(Expr::SpecialForm(SpecialForm::And(exprs)))
}

fn parse_or_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    _span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    let mut exprs = Vec::new();
    for node in &nodes[1..] {
        exprs.push(parse_single_expr(node, expr_parser.clone())?);
    }
    Ok(Expr::SpecialForm(SpecialForm::Or(exprs)))
}

fn parse_progn_form<'s>(
    nodes: &[Node],
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    _span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    let mut exprs = Vec::new();
    for node in &nodes[1..] {
        exprs.push(parse_single_expr(node, expr_parser.clone())?);
    }
    Ok(Expr::SpecialForm(SpecialForm::Progn(exprs)))
}

fn parse_prog1_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "prog1 not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_prog2_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "prog2 not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_quote_form(
    nodes: &[Node],
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() != 2 {
        return Err(ParseError {
            message: "quote requires exactly 1 argument".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let quoted_data = node_to_quoted_data(&nodes[1], false);
    Ok(Expr::SpecialForm(SpecialForm::Quote(Quote {
        kind: QuoteKind::Quote,
        expr: quoted_data,
    })))
}

fn parse_function_form(
    nodes: &[Node],
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    if nodes.len() != 2 {
        return Err(ParseError {
            message: "function requires exactly 1 argument".to_string(),
            span: Some((span.start, span.end)),
        });
    }

    let name = match &nodes[1] {
        Node::Ident(ident) => *ident,
        _ => return Err(ParseError {
            message: "function argument must be a symbol".to_string(),
            span: Some((span.start, span.end)),
        }),
    };

    Ok(Expr::SpecialForm(SpecialForm::Function(Function { name })))
}

// Add placeholder implementations for the remaining forms
fn parse_cond_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "cond not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_while_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "while not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_catch_form(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "catch not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_unwind_protect_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "unwind-protect not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_condition_case_form(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "condition-case not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_save_current_buffer_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "save-current-buffer not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_save_excursion_form(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "save-excursion not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_save_restriction_form<'s>(
    _nodes: &[Node],
    _expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "save-restriction not yet implemented".to_string(),
        span: Some((span.start, span.end)),
    })
}

fn parse_interactive_form(
    _nodes: &[Node],
    span: std::ops::Range<usize>,
) -> Result<Expr, ParseError> {
    Err(ParseError {
        message: "interactive not yet implemented".to_string(),
        span: Some((span.start, span.end)),
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

fn parse_single_expr<'s>(
    node: &Node,
    expr_parser: impl Parser<'s, NodeInput, Expr, ParseError> + Clone,
) -> Result<Expr, ParseError> {
    let node_input = NodeInput::new(vec![node.clone()]);
    expr_parser.parse(node_input).into_result().map_err(|errs| {
        errs.into_iter().next().unwrap_or(ParseError {
            message: "Failed to parse expression".to_string(),
            span: None,
        })
    })
}

pub fn parse_node_to_expr(node: Node) -> Result<Expr, ParseError> {
    let node_input = NodeInput::new(vec![node]);
    node_to_expr().parse(node_input).into_result().map_err(|errs| {
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
