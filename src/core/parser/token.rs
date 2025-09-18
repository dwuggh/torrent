use logos::Logos;

use crate::core::{
    ident::Ident,
    number::{LispCharacter, LispFloat, LispInteger},
    object::LispObject,
    string::LispStr,
    symbol::{LispSymbol, Symbol},
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

impl From<Token> for LispObject {
    fn from(token: Token) -> Self {
        match token {
            Token::Integer(n) => LispObject::Int(LispInteger::new(n)),
            Token::Float(f) => LispObject::Float(LispFloat::new(f)),
            Token::Character(c) => LispObject::Character(LispCharacter::new(c)),
            Token::Str(s) => LispObject::Str(LispStr::new(s)),
            Token::Ident(ident) => {
                let symbol = Symbol::from(ident);
                LispObject::Symbol(LispSymbol(symbol))
            }
            // Special handling for structural tokens - these don't directly convert to objects
            // but could be used in macro expansion contexts
            Token::LParen | Token::RParen | Token::LBracket | Token::RBracket => {
                // Convert structural tokens to symbols for macro processing
                let symbol_name = match token {
                    Token::LParen => "(",
                    Token::RParen => ")",
                    Token::LBracket => "[",
                    Token::RBracket => "]",
                    _ => unreachable!(),
                };
                let symbol = Symbol::from(symbol_name);
                LispObject::Symbol(LispSymbol(symbol))
            }
            // Quote-related tokens become symbols
            Token::Quote => {
                let symbol = Symbol::from("quote");
                LispObject::Symbol(LispSymbol(symbol))
            }
            Token::Backquote => {
                let symbol = Symbol::from("backquote");
                LispObject::Symbol(LispSymbol(symbol))
            }
            Token::Unquote => {
                let symbol = Symbol::from("unquote");
                LispObject::Symbol(LispSymbol(symbol))
            }
            Token::UnquoteSplice => {
                let symbol = Symbol::from("unquote-splicing");
                LispObject::Symbol(LispSymbol(symbol))
            }
            // Comments and errors don't have meaningful object representations
            Token::Comment => {
                let symbol = Symbol::from("comment");
                LispObject::Symbol(LispSymbol(symbol))
            }
            Token::Error => {
                let symbol = Symbol::from("error");
                LispObject::Symbol(LispSymbol(symbol))
            }
        }
    }
}
