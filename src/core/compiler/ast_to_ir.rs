use crate::{
    ast::{Node, Keyword},
    core::{
        compiler::ir::*,
        ident::Ident,
    },
};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum ConversionError {
    InvalidSyntax(String),
    UnsupportedConstruct(String),
    ArityMismatch { expected: usize, got: usize },
    EmptyExpression,
    InvalidLetBinding,
    InvalidLambdaArgs,
    InvalidQuote,
    UnexpectedNode(String),
}

impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConversionError::InvalidSyntax(msg) => write!(f, "Invalid syntax: {}", msg),
            ConversionError::UnsupportedConstruct(msg) => write!(f, "Unsupported construct: {}", msg),
            ConversionError::ArityMismatch { expected, got } => {
                write!(f, "Arity mismatch: expected {}, got {}", expected, got)
            }
            ConversionError::EmptyExpression => write!(f, "Empty expression"),
            ConversionError::InvalidLetBinding => write!(f, "Invalid let binding"),
            ConversionError::InvalidLambdaArgs => write!(f, "Invalid lambda arguments"),
            ConversionError::InvalidQuote => write!(f, "Invalid quote expression"),
            ConversionError::UnexpectedNode(msg) => write!(f, "Unexpected node: {}", msg),
        }
    }
}

impl std::error::Error for ConversionError {}

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
                
                // Try to parse as keyword
                if let Ok(keyword) = Keyword::try_from(name) {
                    Self::convert_keyword(keyword, args)
                } else {
                    // Regular function call
                    Self::convert_call(nodes)
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

    fn convert_keyword(keyword: Keyword, args: &[Node]) -> Result<Expr, ConversionError> {
        match keyword {
            Keyword::If => Self::convert_if(args),
            Keyword::Lambda => Self::convert_lambda(args),
            Keyword::Defun => Self::convert_defun(args),
            Keyword::Defvar => Self::convert_defvar(args),
            Keyword::Set => Self::convert_set(args),
            Keyword::Setq => Self::convert_setq(args),
            Keyword::Setf => Self::convert_setf(args),
            Keyword::Cond => Self::convert_cond(args),
            Keyword::Defmacro => Err(ConversionError::UnsupportedConstruct(
                "defmacro should be handled at macro expansion phase".to_string()
            )),
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

        // For now, just return basic interactive
        Ok(Interactive {
            arg_desc,
            modes: vec![], // TODO: parse mode specifications
        })
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
        // setq is like set but quotes the first argument
        Self::convert_set(args)
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
                Some(Box::new(Self::node_to_expr(args[1].clone())?))
            } else {
                None
            };

            Ok(Expr::Set(Set { symbol: *name, value: value.unwrap_or_else(|| {
                Box::new(Expr::Literal(Literal::Boolean(false))) // nil
            })}))
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

    fn convert_unquote(args: &[Node]) -> Result<Expr, ConversionError> {
        Err(ConversionError::InvalidSyntax(
            "unquote outside of backquote context".to_string()
        ))
    }

    fn convert_unquote_splice(args: &[Node]) -> Result<Expr, ConversionError> {
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
}
