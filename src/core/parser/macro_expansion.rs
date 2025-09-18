use crate::core::{
    cons::Cons,
    env::{Environment, FuncCellType},
    error::{RuntimeError, RuntimeResult},
    object::{LispObject, ObjectRef},
    parser::{
        expr::{Call, Expr}, scope::Scope, token::Token
    },
};
use chumsky::{
    input::{Input, Stream},
    Parser,
};

pub fn macro_expand(expr: &Expr, env: &Environment) {
    let ty = expr.ty.borrow_mut();
    let scope = Scope::Global(env);
}

pub fn macro_expand_call(call: &Call, env: &Environment) -> Option<RuntimeResult<Expr>> {
    let symbol = call.symbol.get().into();
    let result = env
        .load_symbol_with(symbol, Some(FuncCellType::Macro), |val| {
            let ObjectRef::Function(func) = val.as_ref() else {
                return Ok(Err(RuntimeError::WrongType {
                    expected: "function",
                    actual: (val.get_tag()),
                }));
            };
            let args = call
                .args
                .iter()
                .map(|arg| {
                    let obj: LispObject = arg.clone().into();
                    obj.tag()
                })
                .collect::<Vec<_>>();

            let result = func.run(&args, env).and_then(|result| {
                // Convert the macro result to tokens
                let tokens = lisp_object_to_tokens(result.untag());

                // Parse the tokens back into an expression
                let tokens_with_span = tokens
                    .into_iter()
                    .map(|tok| (tok, crate::core::parser::Span::dummy()));
                let stream = Stream::from_iter(tokens_with_span)
                    .map(crate::core::parser::Span::dummy(), |(t, s)| (t, s));

                match crate::core::parser::expr_parser::expr_parser()
                    .parse(stream)
                    .into_result()
                {
                    Ok(expr) => Ok(expr),
                    Err(parse_errors) => Err(RuntimeError::MacroExpansionError {
                        message: format!(
                            "Failed to parse macro expansion result: {:?}",
                            parse_errors
                        ),
                    }),
                }
            });
            Ok(result)
        })
        .ok();

    result
}

/// Convert a LispObject into a sequence of tokens for macro expansion
pub fn lisp_object_to_tokens(obj: LispObject) -> Vec<Token> {
    match obj {
        LispObject::Nil => vec![Token::Ident("nil".into())],
        LispObject::True => vec![Token::Ident("t".into())],
        LispObject::Int(lisp_integer) => vec![Token::Integer(lisp_integer.0)],
        LispObject::Float(lisp_float) => vec![Token::Float(lisp_float.0)],
        LispObject::Character(lisp_character) => vec![Token::Character(lisp_character.value())],
        LispObject::Str(lisp_str) => vec![Token::Str(lisp_str.to_string())],
        LispObject::Symbol(lisp_symbol) => vec![Token::Ident(lisp_symbol.0.ident())],
        LispObject::Vector(lisp_vector) => {
            let mut tokens = vec![Token::LBracket];
            for item in lisp_vector.0.get() {
                let item_obj = item.clone().untag();
                tokens.extend(lisp_object_to_tokens(item_obj));
            }
            tokens.push(Token::RBracket);
            tokens
        }
        LispObject::Cons(lisp_cons) => cons_to_tokens(&lisp_cons.0.get()),
        LispObject::Function(_) => {
            // Functions can't be directly converted to tokens for macro expansion
            vec![Token::Ident("#<function>".into())]
        }
        LispObject::HashTable(_) => {
            // Hash tables can't be directly converted to tokens for macro expansion
            vec![Token::Ident("#<hash-table>".into())]
        }
    }
}

/// Convert a cons cell to tokens, handling proper list structure
fn cons_to_tokens(cons: &Cons) -> Vec<Token> {
    let mut tokens = vec![Token::LParen];

    let vec = cons.to_vec().unwrap();
    // Handle the car
    tokens.push(Token::LParen);
    for obj in vec.into_iter() {
        tokens.extend(lisp_object_to_tokens(obj.untag()));
    }
    tokens.push(Token::RParen);

    tokens
}

/// Iterator adapter for streaming tokens from a LispObject
pub struct LispObjectTokenIterator {
    tokens: std::vec::IntoIter<Token>,
}

impl LispObjectTokenIterator {
    pub fn new(obj: LispObject) -> Self {
        Self {
            tokens: lisp_object_to_tokens(obj).into_iter(),
        }
    }
}

impl Iterator for LispObjectTokenIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}

impl From<LispObject> for LispObjectTokenIterator {
    fn from(obj: LispObject) -> Self {
        Self::new(obj)
    }
}
