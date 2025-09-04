#![allow(dead_code)]
use std::collections::HashSet;

use chumsky::{
    prelude::*,
    text::{digits, Char},
};

use crate::core::{env::Env, symbol::Symbol};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Node {
    Ident(String),
    Sexp(Vec<Node>),
    Vector(Vec<Node>),
    // Value(Value),
    Integer(i64),
    Float(f64),
    Char(char),
    Str(String),
    Unquote,
    UnquoteSplice,
    Backquote,
    Nil,
}

pub fn elisp_parser<'a>() -> impl Parser<'a, &'a str, Vec<Node>, extra::Err<Rich<'a, char>>> {
    // A parser for a single S-expression node.
    // It's recursive to handle nested lists and vectors.
    let node = recursive(|node| {
        // --- Comment and Whitespace Parser ---
        // A comment starts with ';' and goes to the end of the line.
        // let comment = just(';').then(take_until(text::newline())).padded();
        let comment = just(';')
            .ignore_then(none_of('\n').repeated())
            .then_ignore(text::newline().or(end()))
            .padded();
        let comments = comment.repeated().at_least(1);

        // --- Atomic Type Parsers ---
        // `nil` is a special symbol that maps to the Nil node.
        let nil = text::keyword("nil").to(Node::Nil);

        // A character literal starts with '?'
        let char_lit = just('?')
            .ignore_then(
                // Handle escaped characters like ?\n
                just('\\')
                    .ignore_then(
                        just('n')
                            .to('\n')
                            .or(just('t').to('\t'))
                            .or(just('r').to('\r'))
                            .or(just('\\')),
                    )
                    // Handle any other single character
                    .or(any()),
            )
            .map(Node::Char);

        // Numbers can be floats or integers. We parse floats first because
        // they are more specific (e.g., "1.0" would be parsed as an int "1" otherwise).
        let number = {
            let base = just('-').or_not().then(text::digits(10));

            let integer = base
                // .then_ignore(text::whitespace())
                .to_slice()
                .from_str::<i64>()
                .unwrapped();
            let float = base
                .then(just('.').then(digits(10).or_not()))
                .to_slice()
                .from_str::<f64>()
                .unwrapped();
            float.map(Node::Float).or(integer.map(Node::Integer))
        };
        // let number = text::float()
        //     .map(|s: String| Node::Float(s.parse().unwrap()))
        //     .or(text::int(10).map(|s: String| Node::Integer(s.parse().unwrap())));

        // A string is enclosed in double quotes and supports escape sequences.
        let string = {
            let escape = just('\\').ignore_then(choice((
                just('/'),
                just('\\'),
                just('"'),
                just('b').to('\x08'),
                just('f').to('\x0C'),
                just('n').to('\n'),
                just('r').to('\r'),
                just('t').to('\t'),
            )));

            escape
                .or(none_of("\\\""))
                .repeated()
                .collect::<String>()
                .delimited_by(just('"'), just('"'))
                .map(Node::Str)
        };

        // A symbol is a sequence of characters that isn't a number or other syntax.
        // This is a simplified definition for demonstration.
        // let symbol = one_of("+-*/%<>=!?_&^~").or(text::ident()).map(Node::Symbol);
        let symbol = any()
            .filter(|c: &char| !special_chars(*c) && !c.is_whitespace())
            .ignored()
            .repeated()
            .at_least(1)
            .to_slice()
            .map(ToString::to_string)
            .map(Node::Ident);

        // Combine all atomic parsers. Order matters here.
        let atom = nil
            .or(char_lit)
            .or(number)
            .or(string)
            .or(symbol);
            // Atoms can be padded by whitespace or comments.
            // .padded_by(comments.or(text::whitespace()));

        // --- Collection Parsers ---
        // A list is a sequence of nodes enclosed in parentheses.
        let sexp = node
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just('('), just(')'))
            .map(|items| {
                // An empty list `()` is canonical `nil`.
                println!("{items:?}");
                if items.is_empty() {
                    Node::Nil
                } else {
                    Node::Sexp(items)
                }
            });

        // A vector is a sequence of nodes enclosed in square brackets.
        let vector = node
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just('['), just(']'))
            .map(Node::Vector);

        // --- Syntactic Sugar (Quote) Parsers ---
        // These parsers "desugar" the quote syntax into a standard list form.
        let quote = just('\'')
            .ignore_then(node.clone())
            .map(|n| Node::Sexp(vec![Node::Ident("quote".to_string()), n]));

        let backquote = just('`')
            .ignore_then(node.clone())
            .map(|n| Node::Sexp(vec![Node::Backquote, n]));

        let unquote = just(',')
            .ignore_then(
                just('@')
                    .ignore_then(node.clone())
                    .map(|n| (true, n))
                    .or(node.clone().map(|n| (false, n))),
            )
            .map(|(is_splice, n)| {
                let sym = if is_splice {
                    Node::UnquoteSplice
                } else {
                    Node::Unquote
                };
                Node::Sexp(vec![sym, n])
            });

        // The final node parser combines all possibilities.
        // The quote parsers come first as they are special syntax that wrap other nodes.
        quote
            .or(backquote)
            .or(unquote)
            .or(sexp)
            .or(vector)
            .or(atom)
            // .padded()
            .padded_by(comments.or(text::whitespace()))
    });

    // The full parser expects one or more nodes, followed by the end of input.
    node.repeated().collect::<Vec<_>>().then_ignore(end())
}

pub fn special_chars(c: char) -> bool {
    let chars = "#[]()\\\"\',";
    chars.contains(c)
}

impl Node {
    pub fn read_free_vars(&self, env: &Env, result: &mut HashSet<Symbol>) {
        match self {
            Node::Ident(ident) => {
                if let Some(kw) = Keyword::try_from(ident.as_str()).ok() {
                    return
                }
            }
            Node::Sexp(nodes) => {
                for node in nodes.iter() {
                    node.read_free_vars(env, result);
                }
            }
            Node::Vector(nodes) => {
                for node in nodes.iter() {
                    node.read_free_vars(env, result);
                }
            }
            _ => ()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    If,
    Cond,
    Lambda,
    Defun,
    Defmacro,
    Defvar,
    Set,
    Setq,
    Setf,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let result = match value {
            "if" => Self::If,
            "cond" => Self::Cond,
            "lambda" => Self::Lambda,
            "defun" => Self::Defun,
            "defmacro" => Self::Defmacro,
            "defvar" => Self::Defvar,
            "set" => Self::Set,
            "setq" => Self::Setq,
            "setf" => Self::Setf,
            _ => return Err(())
        };
        Ok(result)
    }
}


// --- Test Module ---
#[cfg(test)]
mod tests {
    use super::*;

    fn parse_one(src: &str) -> Result<Node, Vec<Rich<'_, char>>>  {
        elisp_parser()
            .parse(src)
            .into_result()
            .map(|mut nodes| nodes.remove(0))
    }

    #[test]
    fn test_parse_atoms() {
        assert_eq!(parse_one("123").unwrap(), Node::Integer(123));
        assert_eq!(parse_one("-45").unwrap(), Node::Integer(-45));
        assert_eq!(parse_one("1.5").unwrap(), Node::Float(1.5));
        assert_eq!(parse_one("-0.25").unwrap(), Node::Float(-0.25));
        assert_eq!(parse_one("nil").unwrap(), Node::Nil);
        assert_eq!(
            parse_one("my-symbol").unwrap(),
            Node::Ident("my-symbol".to_string())
        );
        assert_eq!(parse_one("+").unwrap(), Node::Ident("+".to_string()));
        assert_eq!(
            parse_one(r#""hello world""#).unwrap(),
            Node::Str("hello world".to_string())
        );
        assert_eq!(
            parse_one(r#""hello\nworld""#).unwrap(),
            Node::Str("hello\nworld".to_string())
        );
        assert_eq!(parse_one("?a").unwrap(), Node::Char('a'));
        assert_eq!(parse_one(r"?\n").unwrap(), Node::Char('\n'));
    }

    #[test]
    fn test_parse_list_and_nil() {
        assert_eq!(parse_one("()").unwrap(), Node::Nil);
        let expected = Node::Sexp(vec![
            Node::Ident("a".to_string()),
            Node::Ident("b".to_string()),
            Node::Integer(123),
        ]);
        assert_eq!(parse_one("(a b 123)").unwrap(), expected);
    }

    #[test]
    fn test_nested_list() {
        let expected = Node::Sexp(vec![
            Node::Ident("a".to_string()),
            Node::Sexp(vec![
                Node::Ident("b".to_string()),
                Node::Ident("c".to_string()),
            ]),
            Node::Ident("d".to_string()),
        ]);
        assert_eq!(parse_one("(a (b c) d)").unwrap(), expected);
    }

    #[test]
    fn test_parse_vector() {
        let expected = Node::Vector(vec![
            Node::Integer(1),
            Node::Float(2.0),
            Node::Str("three".to_string()),
        ]);
        assert_eq!(parse_one("[1 2.0 \"three\"]").unwrap(), expected);
    }

    #[test]
    fn test_comments_and_whitespace() {
        let src = r#"
            ; this is a comment
            ( a   b  ; another comment
              c) ; final comment
        "#;
        let expected = Node::Sexp(vec![
            Node::Ident("a".to_string()),
            Node::Ident("b".to_string()),
            Node::Ident("c".to_string()),
        ]);
        assert_eq!(parse_one(src).unwrap(), expected);
    }

    #[test]
    fn test_desugar_quotes() {
        // 'foo -> (quote foo)
        let expected_quote = Node::Sexp(vec![
            Node::Ident("quote".to_string()),
            Node::Ident("foo".to_string()),
        ]);
        assert_eq!(parse_one("'foo").unwrap(), expected_quote);

        // `foo -> (backquote foo)
        let expected_backquote = Node::Sexp(vec![
            Node::Backquote,
            Node::Ident("foo".to_string()),
        ]);
        assert_eq!(parse_one("`foo").unwrap(), expected_backquote);

        // ,foo -> (unquote foo)
        let expected_unquote = Node::Sexp(vec![
            Node::Unquote,
            Node::Ident("foo".to_string()),
        ]);
        assert_eq!(parse_one(",foo").unwrap(), expected_unquote);

        // ,@foo -> (unquotesplice foo)
        let expected_splice = Node::Sexp(vec![
            Node::UnquoteSplice,
            Node::Ident("foo".to_string()),
        ]);
        assert_eq!(parse_one(",@foo").unwrap(), expected_splice);
    }

    #[test]
    fn test_parse_multiple_expressions() {
        let src = "123 \"hello\" (a b)";
        let ast = elisp_parser().parse(src).unwrap();
        assert_eq!(ast.len(), 3);
        assert_eq!(ast[0], Node::Integer(123));
        assert_eq!(ast[1], Node::Str("hello".to_string()));
        assert_eq!(
            ast[2],
            Node::Sexp(vec![
                Node::Ident("a".to_string()),
                Node::Ident("b".to_string())
            ])
        );
    }
}



