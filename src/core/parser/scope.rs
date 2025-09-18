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
        self.resolve_inner(ident, None)
    }
    
    pub fn resolve_inner(&self, ident: Ident, current_captures: Option<Captures>) -> Var {
        match self {
            Scope::Global(_environment) => {
                // If we have captures, this variable needs to be captured from global scope
                if let Some(captures) = current_captures {
                    let global_var = Var::Global(ident.into());
                    captures.borrow_mut().push(global_var);
                    Var::Captured(ident)
                } else {
                    Var::Global(ident.into())
                }
            }
            Scope::Function {
                args,
                captures,
                parent,
            } => {
                // Check if this is a function argument
                if args.contains(ident) {
                    // Function arguments are directly accessible, no capture needed
                    return Var::Argument(ident);
                }

                // Check if already captured
                if captures
                    .borrow()
                    .iter()
                    .any(|var| Ident::from(*var) == ident)
                {
                    return Var::Captured(ident);
                }

                // Look in parent scope, but now this function needs to capture the variable
                let resolved = parent.resolve_inner(ident, Some(Rc::clone(captures)));
                
                // If the parent resolved it and we're capturing, return captured
                match resolved {
                    Var::Global(_) | Var::Argument(_) | Var::Local(_) | Var::Captured(_) => {
                        Var::Captured(ident)
                    }
                }
            }
            Scope::Lexical { binding, parent } => {
                // Check if this identifier is bound in this lexical scope
                if binding.iter().any(|binding| binding.0 == ident) {
                    // Found in current lexical scope
                    if let Some(captures) = current_captures {
                        // If we're in a function that needs to capture, add to captures
                        captures.borrow_mut().push(Var::Local(ident.into()));
                        Var::Captured(ident)
                    } else {
                        // Direct access to local binding
                        Var::Local(ident.into())
                    }
                } else {
                    // Not found in current scope, continue searching in parent
                    parent.resolve_inner(ident, current_captures)
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
