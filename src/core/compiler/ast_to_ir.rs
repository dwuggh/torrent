use crate::{
    ast::Node,
    core::{compiler::ir::*, ident::Ident, number::{LispCharacter, LispFloat, LispInteger}, string::LispStr},
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

            Node::Integer(n) => Ok(Expr::Literal(Literal::Number(Number::Integer(LispInteger(n))))),

            Node::Float(f) => Ok(Expr::Literal(Literal::Number(Number::Real(LispFloat(f))))),

            Node::Char(c) => Ok(Expr::Literal(Literal::Character(c.into()))),

            Node::Str(s) => Ok(Expr::Literal(Literal::String(LispStr::new(s)))),

            Node::Nil => Ok(Expr::Nil), // or create a Nil literal type

            Node::Vector(nodes) => {
                let exprs = nodes
                    .into_iter()
                    .map(Self::node_to_expr)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::Vector(exprs))
            }

            Node::Sexp(nodes) => Self::convert_sexp(nodes),

            // These should be handled within sexp conversion
            Node::Unquote | Node::UnquoteSplice | Node::Backquote => {
                Err(ConversionError::UnexpectedNode(
                    "Quote operators should be handled in sexp context".to_string(),
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

                match name {
                    // Special forms
                    "if" => Ok(Expr::SpecialForm(SpecialForm::If(Self::convert_if(args)?))),
                    "lambda" => Ok(Expr::SpecialForm(SpecialForm::Lambda(
                        Self::convert_lambda(args)?,
                    ))),
                    "defvar" => Ok(Expr::SpecialForm(SpecialForm::Defvar(
                        Self::convert_defvar(args)?,
                    ))),
                    "defconst" => Ok(Expr::SpecialForm(SpecialForm::Defconst(
                        Self::convert_defconst(args)?,
                    ))),
                    "set" => Ok(Expr::SpecialForm(SpecialForm::Set(Self::convert_set(
                        args,
                    )?))),
                    "setq" => Ok(Expr::SpecialForm(SpecialForm::Setq(Self::convert_setq(
                        args,
                    )?))),
                    "setq-default" => Ok(Expr::SpecialForm(SpecialForm::SetqDefault(
                        Self::convert_setq_default(args)?,
                    ))),
                    "cond" => Ok(Expr::SpecialForm(SpecialForm::Cond(Self::convert_cond(
                        args,
                    )?))),
                    "let" => Ok(Expr::SpecialForm(SpecialForm::Let(Self::convert_let(
                        args,
                    )?))),
                    "let*" => Ok(Expr::SpecialForm(SpecialForm::LetStar(
                        Self::convert_let_star(args)?,
                    ))),
                    "and" => Ok(Expr::SpecialForm(SpecialForm::And(Self::convert_and(
                        args,
                    )?))),
                    "or" => Ok(Expr::SpecialForm(SpecialForm::Or(Self::convert_or(args)?))),
                    "progn" => Ok(Expr::SpecialForm(SpecialForm::Progn(Self::convert_progn(
                        args,
                    )?))),
                    "prog1" => Ok(Expr::SpecialForm(SpecialForm::Prog1(Self::convert_prog1(
                        args,
                    )?))),
                    "prog2" => Ok(Expr::SpecialForm(SpecialForm::Prog2(Self::convert_prog2(
                        args,
                    )?))),
                    "quote" => Ok(Expr::SpecialForm(SpecialForm::Quote(Self::convert_quote(
                        args,
                    )?))),
                    "function" => Ok(Expr::SpecialForm(SpecialForm::Function(
                        Self::convert_function(args)?,
                    ))),
                    "while" => Ok(Expr::SpecialForm(SpecialForm::While(Self::convert_while(
                        args,
                    )?))),
                    "catch" => Ok(Expr::SpecialForm(SpecialForm::Catch(Self::convert_catch(
                        args,
                    )?))),
                    "unwind-protect" => Ok(Expr::SpecialForm(SpecialForm::UnwindProtect(
                        Self::convert_unwind_protect(args)?,
                    ))),
                    "condition-case" => Ok(Expr::SpecialForm(SpecialForm::ConditionCase(
                        Self::convert_condition_case(args)?,
                    ))),
                    "save-current-buffer" => Ok(Expr::SpecialForm(SpecialForm::SaveCurrentBuffer(
                        Self::convert_save_current_buffer(args)?,
                    ))),
                    "save-excursion" => Ok(Expr::SpecialForm(SpecialForm::SaveExcursion(
                        Self::convert_save_excursion(args)?,
                    ))),
                    "save-restriction" => Ok(Expr::SpecialForm(SpecialForm::SaveRestriction(
                        Self::convert_save_restriction(args)?,
                    ))),
                    "interactive" => Ok(Expr::SpecialForm(SpecialForm::Interactive(
                        Self::convert_interactive(args)?,
                    ))),
                    "defmacro" => Err(ConversionError::UnsupportedConstruct(
                        "defmacro should be handled at macro expansion phase".to_string(),
                    )),

                    // Regular function call
                    _ => Self::convert_call(nodes),
                }
            }

            Node::Backquote => Ok(Expr::SpecialForm(SpecialForm::Quote(
                Self::convert_backquote(args)?,
            ))),
            Node::Unquote => Self::convert_unquote(args),
            Node::UnquoteSplice => Self::convert_unquote_splice(args),

            _ => {
                // Function call where the function is an expression
                Self::convert_call(nodes)
            }
        }
    }

    fn convert_if(args: &[Node]) -> Result<If, ConversionError> {
        match args.len() {
            2 => {
                let cond = Arc::new(Self::node_to_expr(args[0].clone())?);
                let then_branch = Arc::new(Self::node_to_expr(args[1].clone())?);
                let else_branch = vec![];
                Ok(If {
                    cond,
                    then: then_branch,
                    els: else_branch,
                })
            }
            3 => {
                let cond = Arc::new(Self::node_to_expr(args[0].clone())?);
                let then_branch = Arc::new(Self::node_to_expr(args[1].clone())?);
                let else_branch = vec![Self::node_to_expr(args[2].clone())?];
                Ok(If {
                    cond,
                    then: then_branch,
                    els: else_branch,
                })
            }
            _ => Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            }),
        }
    }

    fn convert_lambda(args: &[Node]) -> Result<Lambda, ConversionError> {
        if args.is_empty() {
            return Err(ConversionError::InvalidLambdaArgs);
        }

        let params = Self::parse_lambda_params(&args[0])?;
        let body_nodes = &args[1..];

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
                        declare = Some(
                            nodes[1..]
                                .iter()
                                .map(|n| Self::node_to_expr(n.clone()))
                                .collect::<Result<Vec<_>, _>>()?,
                        );
                        body_start += 1;
                    }
                }
            }
        }

        let body = body_nodes[body_start..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        if body.is_empty() {
            return Err(ConversionError::EmptyExpression);
        }

        Ok(Lambda {
            args: params,
            docstring,
            interactive,
            declare,
            body,
            captures: vec![],
        })
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

        let mut modes = Vec::new();
        for arg in args.iter().skip(if arg_desc.is_some() { 1 } else { 0 }) {
            if let Node::Ident(ident) = arg {
                modes.push(*ident);
            }
        }

        Ok(Interactive { arg_desc, modes })
    }

    fn convert_interactive(args: &[Node]) -> Result<Interactive, ConversionError> {
        Self::parse_interactive(args)
    }

    fn convert_let(args: &[Node]) -> Result<Let, ConversionError> {
        if args.len() < 2 {
            return Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            });
        }

        let bindings = Self::parse_let_bindings(&args[0])?;
        let body = args[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Let { bindings, body })
    }

    fn convert_let_star(args: &[Node]) -> Result<LetStar, ConversionError> {
        if args.len() < 2 {
            return Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            });
        }

        let bindings = Self::parse_let_bindings(&args[0])?;
        let body = args[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(LetStar { bindings, body })
    }

    fn parse_let_bindings(node: &Node) -> Result<Vec<(Ident, Option<Expr>)>, ConversionError> {
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

    fn convert_and(args: &[Node]) -> Result<Vec<Expr>, ConversionError> {
        args.iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()
    }

    fn convert_or(args: &[Node]) -> Result<Vec<Expr>, ConversionError> {
        args.iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()
    }

    fn convert_progn(args: &[Node]) -> Result<Vec<Expr>, ConversionError> {
        args.iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()
    }

    fn convert_prog1(args: &[Node]) -> Result<Prog1, ConversionError> {
        if args.is_empty() {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: 0,
            });
        }

        let first = Box::new(Self::node_to_expr(args[0].clone())?);
        let rest = args[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Prog1 { first, rest })
    }

    fn convert_prog2(args: &[Node]) -> Result<Prog2, ConversionError> {
        if args.len() < 2 {
            return Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            });
        }

        let first = Box::new(Self::node_to_expr(args[0].clone())?);
        let second = Box::new(Self::node_to_expr(args[1].clone())?);
        let rest = args[2..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Prog2 {
            first,
            second,
            rest,
        })
    }

    fn convert_quote(args: &[Node]) -> Result<Quote, ConversionError> {
        if args.len() != 1 {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: args.len(),
            });
        }

        let quoted_data = Self::node_to_quoted_data(&args[0], false)?;
        Ok(Quote {
            kind: QuoteKind::Quote,
            expr: quoted_data,
        })
    }

    fn convert_function(args: &[Node]) -> Result<Function, ConversionError> {
        if args.len() != 1 {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: args.len(),
            });
        }

        if let Node::Ident(ident) = &args[0] {
            Ok(Function { name: *ident })
        } else {
            Err(ConversionError::InvalidSyntax(
                "function requires a symbol".to_string(),
            ))
        }
    }

    fn convert_set(args: &[Node]) -> Result<Set, ConversionError> {
        if args.len() != 2 {
            return Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            });
        }

        if let Node::Ident(ident) = &args[0] {
            let value = Box::new(Self::node_to_expr(args[1].clone())?);
            Ok(Set {
                symbol: *ident,
                value,
            })
        } else {
            Err(ConversionError::InvalidSyntax(
                "set requires a symbol as first argument".to_string(),
            ))
        }
    }

    fn convert_setq(args: &[Node]) -> Result<Setq, ConversionError> {
        if args.len() % 2 != 0 {
            return Err(ConversionError::InvalidSyntax(
                "setq requires an even number of arguments (symbol-value pairs)".to_string(),
            ));
        }

        let mut assignments = Vec::new();
        for chunk in args.chunks(2) {
            if let Node::Ident(ident) = &chunk[0] {
                let value = Self::node_to_expr(chunk[1].clone())?;
                assignments.push((*ident, value));
            } else {
                return Err(ConversionError::InvalidSyntax(
                    "setq requires symbols as variable names".to_string(),
                ));
            }
        }

        Ok(Setq { assignments })
    }

    fn convert_setq_default(args: &[Node]) -> Result<SetqDefault, ConversionError> {
        if args.len() % 2 != 0 {
            return Err(ConversionError::InvalidSyntax(
                "setq-default requires an even number of arguments".to_string(),
            ));
        }

        let mut assignments = Vec::new();
        for chunk in args.chunks(2) {
            if let Node::Ident(ident) = &chunk[0] {
                let value = Self::node_to_expr(chunk[1].clone())?;
                assignments.push((*ident, value));
            } else {
                return Err(ConversionError::InvalidSyntax(
                    "setq-default requires symbols as variable names".to_string(),
                ));
            }
        }

        Ok(SetqDefault { assignments })
    }

    fn convert_defvar(args: &[Node]) -> Result<Defvar, ConversionError> {
        if args.is_empty() || args.len() > 3 {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: args.len(),
            });
        }

        if let Node::Ident(name) = &args[0] {
            let value = if args.len() > 1 {
                Some(Box::new(Self::node_to_expr(args[1].clone())?))
            } else {
                None
            };

            let docstring = if args.len() > 2 {
                if let Node::Str(s) = &args[2] {
                    Some(s.clone())
                } else {
                    None
                }
            } else {
                None
            };

            Ok(Defvar {
                symbol: *name,
                value,
                docstring,
            })
        } else {
            Err(ConversionError::InvalidSyntax(
                "defvar requires a symbol as variable name".to_string(),
            ))
        }
    }

    fn convert_defconst(args: &[Node]) -> Result<Defconst, ConversionError> {
        if args.len() < 2 || args.len() > 3 {
            return Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            });
        }

        if let Node::Ident(name) = &args[0] {
            let value = Box::new(Self::node_to_expr(args[1].clone())?);
            let docstring = if args.len() > 2 {
                if let Node::Str(s) = &args[2] {
                    Some(s.clone())
                } else {
                    None
                }
            } else {
                None
            };

            Ok(Defconst {
                symbol: *name,
                value,
                docstring,
            })
        } else {
            Err(ConversionError::InvalidSyntax(
                "defconst requires a symbol as variable name".to_string(),
            ))
        }
    }

    fn convert_cond(args: &[Node]) -> Result<Cond, ConversionError> {
        let mut clauses = Vec::new();

        for clause_node in args {
            if let Node::Sexp(clause) = clause_node {
                if clause.is_empty() {
                    continue;
                }

                let condition = Self::node_to_expr(clause[0].clone())?;
                let body = clause[1..]
                    .iter()
                    .map(|n| Self::node_to_expr(n.clone()))
                    .collect::<Result<Vec<_>, _>>()?;

                clauses.push(CondClause { condition, body });
            } else {
                return Err(ConversionError::InvalidSyntax(
                    "cond clauses must be lists".to_string(),
                ));
            }
        }

        Ok(Cond { clauses })
    }

    fn convert_while(args: &[Node]) -> Result<While, ConversionError> {
        if args.is_empty() {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: 0,
            });
        }

        let condition = Box::new(Self::node_to_expr(args[0].clone())?);
        let body = args[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(While { condition, body })
    }

    fn convert_catch(args: &[Node]) -> Result<Catch, ConversionError> {
        if args.is_empty() {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: 0,
            });
        }

        let tag = Box::new(Self::node_to_expr(args[0].clone())?);
        let body = args[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Catch { tag, body })
    }

    fn convert_unwind_protect(args: &[Node]) -> Result<UnwindProtect, ConversionError> {
        if args.is_empty() {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: 0,
            });
        }

        let protected = Box::new(Self::node_to_expr(args[0].clone())?);
        let cleanup = args[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(UnwindProtect { protected, cleanup })
    }

    fn convert_condition_case(args: &[Node]) -> Result<ConditionCase, ConversionError> {
        if args.len() < 2 {
            return Err(ConversionError::ArityMismatch {
                expected: 2,
                got: args.len(),
            });
        }

        let var = if let Node::Ident(ident) = &args[0] {
            Some(*ident)
        } else if let Node::Nil = &args[0] {
            None
        } else {
            return Err(ConversionError::InvalidSyntax(
                "condition-case var must be a symbol or nil".to_string(),
            ));
        };

        let protected = Box::new(Self::node_to_expr(args[1].clone())?);

        let mut handlers = Vec::new();
        for handler_node in &args[2..] {
            if let Node::Sexp(handler) = handler_node {
                if handler.len() < 2 {
                    return Err(ConversionError::InvalidSyntax(
                        "condition-case handler must have condition and body".to_string(),
                    ));
                }

                let condition = Self::node_to_expr(handler[0].clone())?;
                let body = handler[1..]
                    .iter()
                    .map(|n| Self::node_to_expr(n.clone()))
                    .collect::<Result<Vec<_>, _>>()?;

                handlers.push(ConditionHandler { condition, body });
            } else {
                return Err(ConversionError::InvalidSyntax(
                    "condition-case handlers must be lists".to_string(),
                ));
            }
        }

        Ok(ConditionCase {
            var,
            protected,
            handlers,
        })
    }

    fn convert_save_current_buffer(args: &[Node]) -> Result<SaveCurrentBuffer, ConversionError> {
        let body = args
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(SaveCurrentBuffer { body })
    }

    fn convert_save_excursion(args: &[Node]) -> Result<SaveExcursion, ConversionError> {
        let body = args
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(SaveExcursion { body })
    }

    fn convert_save_restriction(args: &[Node]) -> Result<SaveRestriction, ConversionError> {
        let body = args
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(SaveRestriction { body })
    }

    fn convert_call(nodes: Vec<Node>) -> Result<Expr, ConversionError> {
        if nodes.is_empty() {
            return Err(ConversionError::EmptyExpression);
        }

        let func = Box::new(Self::node_to_expr(nodes[0].clone())?);
        let args = nodes[1..]
            .iter()
            .map(|n| Self::node_to_expr(n.clone()))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Expr::Call(Call { func, args }))
    }

    fn convert_backquote(args: &[Node]) -> Result<Quote, ConversionError> {
        if args.len() != 1 {
            return Err(ConversionError::ArityMismatch {
                expected: 1,
                got: args.len(),
            });
        }

        let quoted_data = Self::node_to_quoted_data(&args[0], true)?;
        Ok(Quote {
            kind: QuoteKind::Backquote,
            expr: quoted_data,
        })
    }

    fn convert_unquote(_args: &[Node]) -> Result<Expr, ConversionError> {
        Err(ConversionError::InvalidSyntax(
            "unquote outside of backquote context".to_string(),
        ))
    }

    fn convert_unquote_splice(_args: &[Node]) -> Result<Expr, ConversionError> {
        Err(ConversionError::InvalidSyntax(
            "unquote-splice outside of backquote context".to_string(),
        ))
    }

    fn node_to_quoted_data(
        node: &Node,
        allow_unquote: bool,
    ) -> Result<QuotedData, ConversionError> {
        match node {
            Node::Ident(ident) => Ok(QuotedData::Symbol(*ident)),
            Node::Integer(n) => Ok(QuotedData::Literal(Literal::Number(Number::Integer(LispInteger(*n))))),
            Node::Float(f) => Ok(QuotedData::Literal(Literal::Number(Number::Real(LispFloat(*f))))),
            Node::Char(c) => Ok(QuotedData::Literal(Literal::Character(LispCharacter::new(*c)))),
            Node::Str(s) => Ok(QuotedData::Literal(Literal::String(LispStr::from_str(
                s,
            )))),
            Node::Nil => Ok(QuotedData::Symbol(Ident::from("nil"))),

            Node::Vector(nodes) => {
                let items = nodes
                    .iter()
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
                let items = nodes
                    .iter()
                    .map(|n| Self::node_to_quoted_data(n, allow_unquote))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(QuotedData::List(items))
            }

            Node::Unquote | Node::UnquoteSplice | Node::Backquote => {
                Err(ConversionError::UnexpectedNode(
                    "Quote operators should be handled in sexp context".to_string(),
                ))
            }
        }
    }
}

// Convenience function
pub fn node_to_ir(node: Node) -> Result<Expr, ConversionError> {
    AstToIrConverter::convert(node)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_literals() {
        let node = Node::Integer(42);
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(
            expr,
            Expr::Literal(Literal::Number(Number::Integer(LispInteger(42))))
        ));
    }

    #[test]
    fn test_simple_call() {
        let node = Node::Sexp(vec![
            Node::Ident("foo".into()),
            Node::Integer(1),
            Node::Integer(2),
        ]);
        let expr = node_to_ir(node).unwrap();
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
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::SpecialForm(SpecialForm::If(_))));
    }

    #[test]
    fn test_let_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("let".into()),
            Node::Sexp(vec![Node::Sexp(vec![
                Node::Ident("x".into()),
                Node::Integer(42),
            ])]),
            Node::Ident("x".into()),
        ]);
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::SpecialForm(SpecialForm::Let(_))));
    }

    #[test]
    fn test_lambda_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("lambda".into()),
            Node::Sexp(vec![Node::Ident("x".into())]),
            Node::Ident("x".into()),
        ]);
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::SpecialForm(SpecialForm::Lambda(_))));
    }

    #[test]
    fn test_quote_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("quote".into()),
            Node::Ident("symbol".into()),
        ]);
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(
            expr,
            Expr::SpecialForm(SpecialForm::Quote(Quote {
                kind: QuoteKind::Quote,
                ..
            }))
        ));
    }

    #[test]
    fn test_backquote_with_unquote() {
        let node = Node::Sexp(vec![
            Node::Backquote,
            Node::Sexp(vec![
                Node::Ident("list".into()),
                Node::Sexp(vec![Node::Unquote, Node::Ident("x".into())]),
            ]),
        ]);
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(
            expr,
            Expr::SpecialForm(SpecialForm::Quote(Quote {
                kind: QuoteKind::Backquote,
                ..
            }))
        ));
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
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::SpecialForm(SpecialForm::Setq(_))));
    }

    #[test]
    fn test_cond_expression() {
        let node = Node::Sexp(vec![
            Node::Ident("cond".into()),
            Node::Sexp(vec![Node::Ident("condition1".into()), Node::Integer(1)]),
            Node::Sexp(vec![Node::Ident("condition2".into()), Node::Integer(2)]),
        ]);
        let expr = node_to_ir(node).unwrap();
        assert!(matches!(expr, Expr::SpecialForm(SpecialForm::Cond(_))));
    }
}
