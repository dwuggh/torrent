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
            // Structural tokens cannot be directly converted to objects
            // They need context (the tokens they contain)
            Token::LParen | Token::RParen | Token::LBracket | Token::RBracket => {
                panic!("Structural tokens cannot be converted to LispObject without context")
            }
        }
    }
}

/// Convert a sequence of tokens between parentheses to a cons list
impl TryFrom<Vec<Token>> for LispObject {
    type Error = &'static str;

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        if tokens.is_empty() {
            return Ok(LispObject::Nil);
        }

        // Check if this is a parenthesized list or bracketed vector
        let (start_token, end_token, is_vector) = match (tokens.first(), tokens.last()) {
            (Some(Token::LParen), Some(Token::RParen)) => (Token::LParen, Token::RParen, false),
            (Some(Token::LBracket), Some(Token::RBracket)) => (Token::LBracket, Token::RBracket, true),
            _ => return Err("Token sequence must be enclosed in matching delimiters"),
        };

        // Extract the inner tokens (excluding delimiters)
        if tokens.len() < 2 {
            return Err("Invalid token sequence");
        }
        
        let inner_tokens = &tokens[1..tokens.len()-1];
        
        if inner_tokens.is_empty() {
            return if is_vector {
                Ok(LispObject::Vector(crate::core::vector::LispVector::new(vec![])))
            } else {
                Ok(LispObject::Nil)
            };
        }

        // Convert inner tokens to LispObjects
        let mut objects = Vec::new();
        let mut i = 0;
        
        while i < inner_tokens.len() {
            match &inner_tokens[i] {
                Token::LParen => {
                    // Find matching closing paren
                    let mut depth = 1;
                    let mut j = i + 1;
                    while j < inner_tokens.len() && depth > 0 {
                        match &inner_tokens[j] {
                            Token::LParen => depth += 1,
                            Token::RParen => depth -= 1,
                            _ => {}
                        }
                        j += 1;
                    }
                    
                    if depth != 0 {
                        return Err("Unmatched parentheses");
                    }
                    
                    // Recursively convert the nested list
                    let nested_tokens = inner_tokens[i..j].to_vec();
                    let nested_obj = LispObject::try_from(nested_tokens)?;
                    objects.push(nested_obj);
                    i = j;
                }
                Token::LBracket => {
                    // Find matching closing bracket
                    let mut depth = 1;
                    let mut j = i + 1;
                    while j < inner_tokens.len() && depth > 0 {
                        match &inner_tokens[j] {
                            Token::LBracket => depth += 1,
                            Token::RBracket => depth -= 1,
                            _ => {}
                        }
                        j += 1;
                    }
                    
                    if depth != 0 {
                        return Err("Unmatched brackets");
                    }
                    
                    // Recursively convert the nested vector
                    let nested_tokens = inner_tokens[i..j].to_vec();
                    let nested_obj = LispObject::try_from(nested_tokens)?;
                    objects.push(nested_obj);
                    i = j;
                }
                Token::RParen | Token::RBracket => {
                    return Err("Unexpected closing delimiter");
                }
                token => {
                    // Convert single token to object
                    objects.push(LispObject::from(token.clone()));
                    i += 1;
                }
            }
        }

        if is_vector {
            // Convert to vector
            let tagged_objects: Vec<_> = objects.into_iter().map(|obj| obj.tag()).collect();
            Ok(LispObject::Vector(crate::core::vector::LispVector::new(tagged_objects)))
        } else {
            // Convert to cons list
            if objects.is_empty() {
                Ok(LispObject::Nil)
            } else {
                let tagged_objects: Vec<_> = objects.into_iter().map(|obj| obj.tag()).collect();
                match crate::core::cons::Cons::from_vec(tagged_objects) {
                    Some(cons) => Ok(LispObject::Cons(crate::core::cons::LispCons(crate::gc::Gc::new(cons)))),
                    None => Ok(LispObject::Nil),
                }
            }
        }
    }
}
