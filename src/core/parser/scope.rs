use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::core::env::Environment;
use crate::core::function::LispFunction;
use crate::core::ident::Ident;
use crate::core::parser::expr::{Args, Binding, Captures, Var};

#[derive(Debug, Clone)]
pub enum Scope<'a> {
    Global(&'a Environment),
    Function {
        args: Rc<Args>,
        captures: Captures,
        parent: Box<Scope<'a>>,
    },
    Lexical {
        binding: Rc<Binding>,
        parent: Box<Scope<'a>>,
    },
}

impl<'a> Scope<'a> {
    pub fn new(env: &'a Environment) -> Self {
        Scope::Global(env)
    }

    pub fn resolve(&self, ident: Ident) -> Var {
        let mut capture_fns = Vec::new();
        self.resolve_inner(ident, &mut capture_fns)
    }
    pub fn resolve_inner(&self, ident: Ident, capture_fns: &mut Vec<Captures>) -> Var {
        match self {
            Scope::Global(environment) => {
                if capture_fns.is_empty() {
                    Var::Global(ident.into())
                } else {
                    for captures in &capture_fns[1..] {
                        captures.borrow_mut().push(Var::Captured(ident));
                    }
                    capture_fns[0].borrow_mut().push(Var::Global(ident.into()));
                    Var::Captured(ident)
                }
            }
            Scope::Function {
                args,
                captures,
                parent,
            } => {
                if args.contains(ident) {
                    // function argument should not be captured, direct return
                    return Var::Argument(ident);
                }

                if captures
                    .borrow()
                    .iter()
                    .any(|var| Ident::from(*var) == ident)
                {
                    return Var::Captured(ident);
                }

                capture_fns.push(Rc::clone(captures));

                parent.resolve_inner(ident, capture_fns)
            }
            Scope::Lexical { binding, parent } => {
                if binding.iter().any(|binding| binding.0 == ident) {
                    for captures in capture_fns.iter() {
                        captures.borrow_mut().push(Var::Captured(ident));
                    }
                    return Var::Captured(ident);
                } else {
                    parent.resolve_inner(ident, capture_fns)
                }
            }
        }
    }
}

impl Args {
    fn contains(&self, ident: Ident) -> bool {
        self.normal.contains(&ident)
            || self
                .optional
                .as_ref()
                .is_some_and(|op_args| op_args.contains(&ident))
            || self.rest.is_some_and(|rest| rest == ident)
    }
}
