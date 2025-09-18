use logos::Logos;

use crate::core::ident::Ident;

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
