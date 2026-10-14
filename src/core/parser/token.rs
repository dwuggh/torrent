use logos::Logos;

use crate::core::{
    ident::Ident,
    number::{LispCharacter, LispInteger},
};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\r\n\f]+")] // Skip whitespace
pub enum Token {
    // Single-character tokens for lists and vectors
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    // Comments start with a semicolon and go to the end of the line
    #[regex(r";.*")]
    Comment,

    // Numbers (integers and floats)
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok(), priority=20)]
    Integer(i64),
    #[regex(r"-?[0-9]+(\.[0-9]+)", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    // NEW: Character token
    #[regex(r#"\?(\\[^ \t\r\n\f]|.)"#, |lex| {
        let slice = lex.slice();
        let char_part = &slice[1..]; // Skip the leading '?'
        if char_part.starts_with('\\') {
            // Handle escaped characters
            match char_part.chars().nth(1) {
                Some('n') => Some('\n'),
                Some('t') => Some('\t'),
                Some('r') => Some('\r'),
                Some('\\') => Some('\\'),
                // Add other escapes as needed
                Some(c) => Some(c), // For things like `\ ` (space)
                None => None,
            }
        } else {
            // Handle literal characters
            char_part.chars().next()
        }
    })]
    Character(char),

    // Symbols/Identifiers. This is a broad category in Lisp.
    // It includes keywords like `defun` and variables.
    // The regex allows for most valid symbol characters.
    #[regex(r"[a-zA-Z_?!*+=<>&~|-][a-zA-Z0-9_?!*+=<>&~|-]*", |lex| {
        let slice = lex.slice();
        Some(slice.into())
    }
    )]
    Ident(Ident),

    // Strings are enclosed in double quotes, with support for \" and \\ escapes.
    // A callback is used to process the escape sequences.
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let slice = lex.slice();
        // Remove the outer quotes and handle escapes
        let s = &slice[1..slice.len() - 1];
        Some(s.replace("\\\"", "\"").replace("\\\\", "\\"))
    })]
    Str(String),

    // Quoted forms like 'foo, `foo, ,foo, and ,@foo
    #[token("'")]
    Quote,
    #[token("`")]
    Backquote,
    #[token(",")]
    Unquote,
    #[token(",@")]
    UnquoteSplice,

    Error,
}

impl Token {
    pub fn to_object(&self, ctx: &mut crate::gc::GcContext) -> crate::core::object::Object {
        use crate::core::parser::expr::{Datum, Literal, Number};
        let datum = match self {
            Self::Integer(n) => Datum::Literal(Literal::Number(Number::Integer(LispInteger(*n)))),
            Self::Float(f) => Datum::Literal(Literal::Number(Number::Real(*f))),
            Self::Character(c) => Datum::Literal(Literal::Character(LispCharacter::new(*c))),
            Self::Str(s) => Datum::Literal(Literal::String(s.clone())),
            Self::Ident(ident) => Datum::Symbol((*ident).into()),
            Self::Quote => Datum::Symbol("quote".into()),
            Self::Backquote => Datum::Symbol("backquote".into()),
            Self::Unquote => Datum::Symbol("unquote".into()),
            Self::UnquoteSplice => Datum::Symbol("unquote-splicing".into()),
            Self::Comment => Datum::Symbol("comment".into()),
            Self::Error => Datum::Symbol("error".into()),
            _ => panic!("Structural tokens require their contents"),
        };
        datum.to_object(ctx)
    }

    pub fn objects_from_tokens(
        tokens: &[Self],
        ctx: &mut crate::gc::GcContext,
    ) -> Result<crate::core::object::Object, &'static str> {
        use crate::core::{cons::LispCons, tag::Tag, vector::LispVector};
        if tokens.is_empty() {
            return Ok(crate::core::object::nil());
        }
        let vector = match (tokens.first(), tokens.last()) {
            (Some(Self::LParen), Some(Self::RParen)) => false,
            (Some(Self::LBracket), Some(Self::RBracket)) => true,
            _ => return Err("Token sequence must be enclosed in matching delimiters"),
        };
        let mut roots = Vec::new();
        let mut i = 1;
        while i < tokens.len() - 1 {
            let value = match &tokens[i] {
                Self::LParen | Self::LBracket => {
                    let mut delimiters = Vec::new();
                    let mut end = i;
                    loop {
                        match tokens.get(end) {
                            Some(Self::LParen) => delimiters.push(Self::RParen),
                            Some(Self::LBracket) => delimiters.push(Self::RBracket),
                            Some(token @ (Self::RParen | Self::RBracket)) => {
                                if delimiters.pop().as_ref() != Some(token) {
                                    return Err("Mismatched delimiter");
                                }
                            }
                            None => return Err("Unmatched delimiter"),
                            _ => {}
                        }
                        end += 1;
                        if delimiters.is_empty() {
                            break;
                        }
                    }
                    let value = Self::objects_from_tokens(&tokens[i..end], ctx)?;
                    i = end;
                    value
                }
                Self::RParen | Self::RBracket => return Err("Unexpected closing delimiter"),
                token => {
                    i += 1;
                    token.to_object(ctx)
                }
            };
            roots.push(ctx.root(value));
        }
        let values = roots.iter().map(|root| *root.get(ctx)).collect::<Vec<_>>();
        if vector {
            Ok(LispVector::new(values, ctx).tag())
        } else {
            let roots = ctx.root_slice(&values);
            Ok(LispCons::list_from_rooted_slice(&roots, ctx))
        }
    }
}
