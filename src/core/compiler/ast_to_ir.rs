use crate::{
    ast::Node,
    core::{
        compiler::ir::*,
        ident::Ident,
    },
};
use std::sync::Arc;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ConversionError {
    #[error("Invalid syntax: {0}")]
    InvalidSyntax(String),
    
    #[error("Unsupported construct: {0}")]
    UnsupportedConstruct(String),
    
    #[error("Arity mismatch: expected {expected}, got {got}")]
    ArityMismatch { expected: usize, got: usize },
    
    #[error("Empty expression")]
    EmptyExpression,
    
    #[error("Invalid let binding")]
    InvalidLetBinding,
    
    #[error("Invalid lambda arguments")]
    InvalidLambdaArgs,
    
    #[error("Invalid quote expression")]
    InvalidQuote,
    
    #[error("Unexpected node: {0}")]
    UnexpectedNode(String),
    
    #[error("Missing keyword: {0}")]
    MissingKeyword(String),
    
    #[error("Invalid argument type in position {position}")]
    InvalidArgumentType { position: usize },
}

pub struct AstToIrConverter;

impl AstToIrConverter {
    pub fn convert(node: Node) -> Result<Expr, ConversionError> {
        Self::node_to_expr(node)
    }

    fn node_to_expr(node: Node) -> Result<Expr, ConversionError> {
        match node {
            Node::Ident(ident) => Ok(Expr::Symbol(ident)),
            
            Node::Integer(n) => Ok(Expr::Literal(Literal::Number(Number::FixedInteger(n)))),
            
            Node::Float(f) => Ok(Expr::Literal(Literal::Number(Number::Real(f)))),
            
            Node::Char(c) => Ok(Expr::Literal(Literal::Character(c))),
            
            Node::Str(s) => Ok(Expr::Literal(Literal::String(s))),
            
            Node::Nil => Ok(Expr::Literal(Literal::Boolean(false))), // or create a Nil literal type
            
            Node::Vector(nodes) => {
                let exprs = nodes.into_iter()
                    .map(Self::node_to_expr)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::Vector(exprs))
            }
            
            Node::Sexp(nodes) => Self::convert_sexp(nodes),
            
            // These should be handled within sexp conversion
            Node::Unquote | Node::UnquoteSplice | Node::Backquote => {
                Err(ConversionError::UnexpectedNode(
                    "Quote operators should be handled in sexp context".to_string()
                ))
            }
        }
    }

    fn convert_sexp(nodes: Vec<Node>) -> Result<Expr, ConversionError> {
        if nodes.is_empty() {
            return Err(ConversionError::EmptyExpression);
        }

        let first = &nodes[0];
        let args = &nodes[1..];

        match first {
            Node::Ident(ident) => {
                let name = ident.text();
                
                // Direct string matching instead of Keyword enum
                match name {
                    // Special forms
                    "if" => Self::convert_if(args),
                    "lambda" => Self::convert_lambda(args),
                    "defun" => Self::convert_defun(args),
                    "defvar" => Self::convert_defvar(args),
                    "set" => Self::convert_set(args),
                    "setq" => Self::convert_setq(args),
                    "setf" => Self::convert_setf(args),
                    "cond" => Self::convert_cond(args),
                    "let" => Self::convert_let(args),
                    "progn" => Self::convert_progn(args),
                    "and" => Self::convert_and(args),
                    "or" => Self::convert_or(args),
                    "quote" => Self::convert_quote(args),
                    "defmacro" => Err(ConversionError::UnsupportedConstruct(
                        "defmacro should be handled at macro expansion phase".to_string()
                    )),
                    
                    // Regular function call
                    _ => Self::convert_call(nodes),
                }
            }
            
            Node::Backquote => Self::convert_backquote(args),
            Node::Unquote => Self::convert_unquote(args),
            Node::UnquoteSplice => Self::convert_unquote_splice(args),
            
            _ => {
                // Function call where the function is an expression
                Self::convert_call(nodes)
            }
        }
    }


    fn convert_if(args: &[Node]) -> Result<Expr, ConversionError> {
        match args.len() {
            2 => {
                let cond = Arc::new(Self::node_to_expr(args[0].clone())?);
                let then_branch = Arc::new(Self::node_to_expr(args[1].clone())?);
                let else_branch = vec![]; // No else clause
                Ok(Expr::If(If { cond, then: then_branch, els: else_branch }))
            }
            3 => {
                let cond = Arc::new(Self::node_to_expr(args[0].clone())?);
                let then_branch = Arc::new(Self::node_to_expr(args[1].clone())?);
                let else_branch = vec![Self::node_to_expr(args[2].clone())?];
                Ok(Expr::If(If { cond, then: then_branch, els: else_branch }))
            }
            _ => Err(ConversionError::ArityMismatch { 
                expected: 2, 
                got: args.len() 
            }),
        }
    }

    fn convert_lambda(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.is_empty() {
            return Err(ConversionError::InvalidLambdaArgs);
        }

        let params = Self::parse_lambda_params(&args[0])?;
        let body_nodes = &args[1..];
        
        // Check for docstring and interactive declarations
        let mut body_start = 0;
        let mut docstring = None;
        let mut interactive = None;
        let mut declare = None;

        // Parse optional docstring
        if body_start < body_nodes.len() {
            if let Node::Str(s) = &body_nodes[body_start] {
                docstring = Some(s.clone());
                body_start += 1;
            }
        }

        // Parse optional interactive declaration
        if body_start < body_nodes.len() {
            if let Node::Sexp(nodes) = &body_nodes[body_start] {
                if let Some(Node::Ident(ident)) = nodes.first() {
                    if ident.text() == "interactive" {
                        interactive = Some(Self::parse_interactive(&nodes[1..])?);
                        body_start += 1;
                    }
                }
            }
        }

        // Parse optional declare forms
        if body_start < body_nodes.len() {
            if let Node::Sexp(nodes) = &body_nodes[body_start] {
                if let Some(Node::Ident(ident)) = nodes.first() {
                    if ident.text() == "declare" {
                        declare = Some(nodes[1..].iter()
                            .map(|n| Self::node_to_expr(n.clone()))
                            .collect::<Result<Vec<_>, _>>()?);
                        body_start += 1;
                    }
                }
            }
        }

        let body = body_nodes[body_start..].iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        if body.is_empty() {
            return Err(ConversionError::EmptyExpression);
        }

        Ok(Expr::Lambda(Lambda {
            args: params,
            docstring,
            interactive,
            declare,
            body,
            captures: vec![], // Will be filled by closure analysis
        }))
    }

    fn parse_lambda_params(node: &Node) -> Result<Vec<Arg>, ConversionError> {
        match node {
            Node::Nil => Ok(vec![]),
            Node::Sexp(nodes) => {
                let mut params = Vec::new();
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
                                    params.push(Arg {
                                        ty: arg_type,
                                        ident: *ident,
                                    });
                                }
                            }
                        }
                        _ => return Err(ConversionError::InvalidLambdaArgs),
                    }
                }
                Ok(params)
            }
            _ => Err(ConversionError::InvalidLambdaArgs),
        }
    }

    fn parse_interactive(args: &[Node]) -> Result<Interactive, ConversionError> {
        let arg_desc = if let Some(Node::Str(s)) = args.first() {
            Some(s.clone())
        } else {
            None
        };

        // Parse mode specifications if present
        let mut modes = Vec::new();
        for arg in args.iter().skip(if arg_desc.is_some() { 1 } else { 0 }) {
            if let Node::Ident(ident) = arg {
                modes.push(*ident);
            }
        }

        Ok(Interactive { arg_desc, modes })
    }

    fn convert_let(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.len() < 2 {
            return Err(ConversionError::ArityMismatch { 
                expected: 2, 
                got: args.len() 
            });
        }

        let bindings = Self::parse_let_bindings(&args[0])?;
        let body = args[1..].iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Expr::Let(Let { bindings, body }))
    }

    fn parse_let_bindings(node: &Node) -> Result<Vec<(Ident, Option<Expr>)>, ConversionError> {
        match node {
            Node::Nil => Ok(vec![]),
            Node::Sexp(nodes) => {
                let mut bindings = Vec::new();
                for binding_node in nodes {
                    match binding_node {
                        Node::Ident(ident) => {
                            // Simple binding with no value (defaults to nil)
                            bindings.push((*ident, None));
                        }
                        Node::Sexp(binding_parts) => {
                            if binding_parts.len() != 2 {
                                return Err(ConversionError::InvalidLetBinding);
                            }
                            if let Node::Ident(ident) = &binding_parts[0] {
                                let value = Self::node_to_expr(binding_parts[1].clone())?;
                                bindings.push((*ident, Some(value)));
                            } else {
                                return Err(ConversionError::InvalidLetBinding);
                            }
                        }
                        _ => return Err(ConversionError::InvalidLetBinding),
                    }
                }
                Ok(bindings)
            }
            _ => Err(ConversionError::InvalidLetBinding),
        }
    }

    fn convert_progn(args: &[Node]) -> Result<Expr, ConversionError> {
        let exprs = args.iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::Progn(exprs))
    }

    fn convert_and(args: &[Node]) -> Result<Expr, ConversionError> {
        let exprs = args.iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::And(exprs))
    }

    fn convert_or(args: &[Node]) -> Result<Expr, ConversionError> {
        let exprs = args.iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::Or(exprs))
    }

    fn convert_quote(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.len() != 1 {
            return Err(ConversionError::ArityMismatch { 
                expected: 1, 
                got: args.len() 
            });
        }

        let quoted_data = Self::node_to_quoted_data(&args[0], false)?;
        Ok(Expr::Quote(Quote {
            kind: QuoteKind::Quote,
            expr: quoted_data,
        }))
    }

    fn convert_set(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.len() != 2 {
            return Err(ConversionError::ArityMismatch { 
                expected: 2, 
                got: args.len() 
            });
        }

        if let Node::Ident(ident) = &args[0] {
            let value = Box::new(Self::node_to_expr(args[1].clone())?);
            Ok(Expr::Set(Set { symbol: *ident, value }))
        } else {
            Err(ConversionError::InvalidSyntax(
                "set requires a symbol as first argument".to_string()
            ))
        }
    }

    fn convert_setq(args: &[Node]) -> Result<Expr, ConversionError> {
        // setq can handle multiple symbol-value pairs
        if args.len() % 2 != 0 {
            return Err(ConversionError::InvalidSyntax(
                "setq requires an even number of arguments (symbol-value pairs)".to_string()
            ));
        }

        if args.is_empty() {
            return Ok(Expr::Literal(Literal::Boolean(false))); // nil
        }

        let mut assignments = Vec::new();
        for chunk in args.chunks(2) {
            if let Node::Ident(ident) = &chunk[0] {
                let value = Box::new(Self::node_to_expr(chunk[1].clone())?);
                assignments.push(Expr::Set(Set { symbol: *ident, value }));
            } else {
                return Err(ConversionError::InvalidSyntax(
                    "setq requires symbols as variable names".to_string()
                ));
            }
        }

        if assignments.len() == 1 {
            Ok(assignments.into_iter().next().unwrap())
        } else {
            Ok(Expr::Progn(assignments))
        }
    }

    fn convert_setf(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.len() != 2 {
            return Err(ConversionError::ArityMismatch { 
                expected: 2, 
                got: args.len() 
            });
        }

        if let Node::Ident(ident) = &args[0] {
            let function = Box::new(Self::node_to_expr(args[1].clone())?);
            Ok(Expr::Fset(Fset { symbol: *ident, function }))
        } else {
            Err(ConversionError::InvalidSyntax(
                "fset requires a symbol as first argument".to_string()
            ))
        }
    }

    fn convert_defun(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.len() < 3 {
            return Err(ConversionError::ArityMismatch { 
                expected: 3, 
                got: args.len() 
            });
        }

        if let Node::Ident(name) = &args[0] {
            // Convert defun to fset with lambda
            let mut lambda_args = vec![args[1].clone()];
            lambda_args.extend_from_slice(&args[2..]);
            let lambda = Self::convert_lambda(&lambda_args)?;
            
            Ok(Expr::Fset(Fset {
                symbol: *name,
                function: Box::new(lambda),
            }))
        } else {
            Err(ConversionError::InvalidSyntax(
                "defun requires a symbol as function name".to_string()
            ))
        }
    }

    fn convert_defvar(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.is_empty() || args.len() > 3 {
            return Err(ConversionError::ArityMismatch { 
                expected: 1, 
                got: args.len() 
            });
        }

        if let Node::Ident(name) = &args[0] {
            let value = if args.len() > 1 {
                Box::new(Self::node_to_expr(args[1].clone())?)
            } else {
                Box::new(Expr::Literal(Literal::Boolean(false))) // nil
            };

            // TODO: Handle optional docstring in args[2]
            Ok(Expr::Set(Set { symbol: *name, value }))
        } else {
            Err(ConversionError::InvalidSyntax(
                "defvar requires a symbol as variable name".to_string()
            ))
        }
    }

    fn convert_cond(args: &[Node]) -> Result<Expr, ConversionError> {
        // Convert cond to nested if expressions
        if args.is_empty() {
            return Ok(Expr::Literal(Literal::Boolean(false))); // nil
        }

        let mut result = Expr::Literal(Literal::Boolean(false)); // default nil

        // Process clauses in reverse order
        for clause_node in args.iter().rev() {
            if let Node::Sexp(clause) = clause_node {
                if clause.is_empty() {
                    continue;
                }

                let condition = Self::node_to_expr(clause[0].clone())?;
                let body = if clause.len() > 1 {
                    if clause.len() == 2 {
                        Self::node_to_expr(clause[1].clone())?
                    } else {
                        Expr::Progn(clause[1..].iter()
                            .map(|n| Self::node_to_expr(n.clone()))
                            .collect::<Result<Vec<_>, _>>()?)
                    }
                } else {
                    condition.clone() // If no body, return the condition value
                };

                result = Expr::If(If {
                    cond: Arc::new(condition),
                    then: Arc::new(body),
                    els: vec![result],
                });
            } else {
                return Err(ConversionError::InvalidSyntax(
                    "cond clauses must be lists".to_string()
                ));
            }
        }

        Ok(result)
    }

    fn convert_call(nodes: Vec<Node>) -> Result<Expr, ConversionError> {
        if nodes.is_empty() {
            return Err(ConversionError::EmptyExpression);
        }

        let func = Box::new(Self::node_to_expr(nodes[0].clone())?);
        let args = nodes[1..].iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Expr::Call(Call { func, args }))
    }

    fn convert_backquote(args: &[Node]) -> Result<Expr, ConversionError> {
        if args.len() != 1 {
            return Err(ConversionError::ArityMismatch { 
                expected: 1, 
                got: args.len() 
            });
        }

        let quoted_data = Self::node_to_quoted_data(&args[0], true)?;
        Ok(Expr::Quote(Quote {
            kind: QuoteKind::Backquote,
            expr: quoted_data,
        }))
    }

    fn convert_unquote(_args: &[Node]) -> Result<Expr, ConversionError> {
        Err(ConversionError::InvalidSyntax(
            "unquote outside of backquote context".to_string()
        ))
    }

    fn convert_unquote_splice(_args: &[Node]) -> Result<Expr, ConversionError> {
        Err(ConversionError::InvalidSyntax(
            "unquote-splice outside of backquote context".to_string()
        ))
    }

    fn node_to_quoted_data(node: &Node, allow_unquote: bool) -> Result<QuotedData, ConversionError> {
        match node {
            Node::Ident(ident) => Ok(QuotedData::Symbol(*ident)),
            Node::Integer(n) => Ok(QuotedData::Literal(Literal::Number(Number::FixedInteger(*n)))),
            Node::Float(f) => Ok(QuotedData::Literal(Literal::Number(Number::Real(*f)))),
            Node::Char(c) => Ok(QuotedData::Literal(Literal::Character(*c))),
            Node::Str(s) => Ok(QuotedData::Literal(Literal::String(s.clone()))),
            Node::Nil => Ok(QuotedData::Literal(Literal::Boolean(false))),
            
            Node::Vector(nodes) => {
                let items = nodes.iter()
                    .map(|n| Self::node_to_quoted_data(n, allow_unquote))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(QuotedData::Vector(items))
            }
            
            Node::Sexp(nodes) => {
                // Handle unquote/unquote-splice
                if allow_unquote && !nodes.is_empty() {
                    match &nodes[0] {
                        Node::Unquote => {
                            if nodes.len() != 2 {
                                return Err(ConversionError::InvalidQuote);
                            }
                            let expr = Self::node_to_expr(nodes[1].clone())?;
                            return Ok(QuotedData::Unquote(Box::new(expr)));
                        }
                        Node::UnquoteSplice => {
                            if nodes.len() != 2 {
                                return Err(ConversionError::InvalidQuote);
                            }
                            let expr = Self::node_to_expr(nodes[1].clone())?;
                            return Ok(QuotedData::UnquoteSplice(Box::new(expr)));
                        }
                        _ => {}
                    }
                }

                // Regular list
                let items = nodes.iter()
                    .map(|n| Self::node_to_quoted_data(n, allow_unquote))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(QuotedData::List(items))
            }
            
            Node::Unquote | Node::UnquoteSplice | Node::Backquote => {
                Err(ConversionError::UnexpectedNode(
                    "Quote operators should be handled in sexp context".to_string()
                ))
            }
        }
    }
}

// Convenience function
pub fn ast_to_ir(node: Node) -> Result<Expr, ConversionError> {
    AstToIrConverter::convert(node)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_literals() {
        let node = Node::Integer(42);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Literal(Literal::Number(Number::FixedInteger(42)))));
    }

    #[test]
    fn test_simple_call() {
        let node = Node::Sexp(vec![
            Node::Ident("foo".into()),
            Node::Integer(1),
            Node::Integer(2),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Call(_)));
    }

    #[test]
    fn test_if_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("if".into()),
            Node::Ident("condition".into()),
            Node::Integer(1),
            Node::Integer(2),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::If(_)));
    }

    #[test]
    fn test_let_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("let".into()),
            Node::Sexp(vec![
                Node::Sexp(vec![
                    Node::Ident("x".into()),
                    Node::Integer(42),
                ]),
            ]),
            Node::Ident("x".into()),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Let(_)));
    }

    #[test]
    fn test_lambda_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("lambda".into()),
            Node::Sexp(vec![Node::Ident("x".into())]),
            Node::Ident("x".into()),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Lambda(_)));
    }

    #[test]
    fn test_quote_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("quote".into()),
            Node::Ident("symbol".into()),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Quote(Quote { kind: QuoteKind::Quote, .. })));
    }

    #[test]
    fn test_backquote_with_unquote() {
        let node = Node::Sexp(vec![
            Node::Backquote,
            Node::Sexp(vec![
                Node::Ident("list".into()),
                Node::Sexp(vec![
                    Node::Unquote,
                    Node::Ident("x".into()),
                ]),
            ]),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Quote(Quote { kind: QuoteKind::Backquote, .. })));
    }

    #[test]
    fn test_setq_multiple_pairs() {
        let node = Node::Sexp(vec![
            Node::Ident("setq".into()),
            Node::Ident("x".into()),
            Node::Integer(1),
            Node::Ident("y".into()),
            Node::Integer(2),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::Progn(_)));
    }

    #[test]
    fn test_cond_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("cond".into()),
            Node::Sexp(vec![
                Node::Ident("condition1".into()),
                Node::Integer(1),
            ]),
            Node::Sexp(vec![
                Node::Ident("condition2".into()),
                Node::Integer(2),
            ]),
        ]);
        let expr = ast_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::If(_)));
    }
}
