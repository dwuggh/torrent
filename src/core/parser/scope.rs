use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::core::env::Environment;
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

    fn resolve_inner(&self, ident: Ident, current_captures: Option<Captures>) -> Var {
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

                // let current_captures = current_captures.or(Some(Rc::clone(captures)));
                let result = parent.resolve_inner(ident, Some(Rc::clone(captures)));
                match result {
                    Var::Captured(ident) => {
                        current_captures
                            .map(|captures| captures.borrow_mut().push(Var::Captured(ident)));
                    }
                    Var::Argument(ident) => {
                        current_captures
                            .map(|captures| captures.borrow_mut().push(Var::Captured(ident)));
                    }
                    _ => unreachable!(),
                }

                result
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::env::Environment;
    use crate::core::ident::Ident;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn create_test_env() -> Environment {
        Environment::new()
    }

    fn create_test_args(normal: Vec<&str>, optional: Option<Vec<&str>>, rest: Option<&str>) -> Rc<Args> {
        Rc::new(Args {
            normal: normal.into_iter().map(|s| Ident::from(s)).collect(),
            optional: optional.map(|opt| opt.into_iter().map(|s| Ident::from(s)).collect()),
            rest: rest.map(|s| Ident::from(s)),
        })
    }

    fn create_test_binding(bindings: Vec<(&str, bool)>) -> Rc<Binding> {
        Rc::new(
            bindings
                .into_iter()
                .map(|(name, has_value)| {
                    let ident = Ident::from(name);
                    let value = if has_value { 
                        Some(crate::core::parser::expr::Expr::new(
                            crate::core::parser::expr::ExprType::Nil,
                            crate::core::parser::Span::dummy()
                        ))
                    } else { 
                        None 
                    };
                    (ident, value)
                })
                .collect(),
        )
    }

    #[test]
    fn test_global_scope_resolution() {
        let env = create_test_env();
        let scope = Scope::Global(&env);
        
        let ident = Ident::from("global-var");
        let result = scope.resolve(ident);
        
        match result {
            Var::Global(symbol) => {
                assert_eq!(symbol.ident(), ident);
            }
            _ => panic!("Expected Global variant, got {:?}", result),
        }
    }

    #[test]
    fn test_function_argument_resolution() {
        let env = create_test_env();
        let args = create_test_args(vec!["x", "y"], None, None);
        let captures = Rc::new(RefCell::new(Vec::new()));
        
        let scope = Scope::Function {
            args,
            captures,
            parent: Box::new(Scope::Global(&env)),
        };
        
        // Test resolving function argument
        let x_ident = Ident::from("x");
        let result = scope.resolve(x_ident);
        
        match result {
            Var::Argument(ident) => {
                assert_eq!(ident, x_ident);
            }
            _ => panic!("Expected Argument variant, got {:?}", result),
        }
        
        // Test resolving non-argument (should go to parent)
        let z_ident = Ident::from("z");
        let result = scope.resolve(z_ident);
        
        match result {
            Var::Global(_) => {
                // Should resolve to global since not found in function args
            }
            _ => panic!("Expected Global variant for non-argument, got {:?}", result),
        }
    }

    #[test]
    fn test_function_optional_argument_resolution() {
        let env = create_test_env();
        let args = create_test_args(vec!["x"], Some(vec!["y", "z"]), None);
        let captures = Rc::new(RefCell::new(Vec::new()));
        
        let scope = Scope::Function {
            args,
            captures,
            parent: Box::new(Scope::Global(&env)),
        };
        
        // Test normal argument
        let x_result = scope.resolve(Ident::from("x"));
        assert!(matches!(x_result, Var::Argument(_)));
        
        // Test optional arguments
        let y_result = scope.resolve(Ident::from("y"));
        assert!(matches!(y_result, Var::Argument(_)));
        
        let z_result = scope.resolve(Ident::from("z"));
        assert!(matches!(z_result, Var::Argument(_)));
    }

    #[test]
    fn test_function_rest_argument_resolution() {
        let env = create_test_env();
        let args = create_test_args(vec!["x"], None, Some("rest"));
        let captures = Rc::new(RefCell::new(Vec::new()));
        
        let scope = Scope::Function {
            args,
            captures,
            parent: Box::new(Scope::Global(&env)),
        };
        
        // Test rest argument
        let rest_result = scope.resolve(Ident::from("rest"));
        assert!(matches!(rest_result, Var::Argument(_)));
        
        // Test normal argument
        let x_result = scope.resolve(Ident::from("x"));
        assert!(matches!(x_result, Var::Argument(_)));
    }

    #[test]
    fn test_lexical_scope_resolution() {
        let env = create_test_env();
        let binding = create_test_binding(vec![("local-var", true), ("another-var", false)]);
        
        let scope = Scope::Lexical {
            binding,
            parent: Box::new(Scope::Global(&env)),
        };
        
        // Test resolving local binding
        let local_ident = Ident::from("local-var");
        let result = scope.resolve(local_ident);
        
        match result {
            Var::Local(ident) => {
                assert_eq!(ident, local_ident);
            }
            _ => panic!("Expected Local variant, got {:?}", result),
        }
        
        // Test resolving another local binding
        let another_ident = Ident::from("another-var");
        let result = scope.resolve(another_ident);
        
        match result {
            Var::Local(ident) => {
                assert_eq!(ident, another_ident);
            }
            _ => panic!("Expected Local variant, got {:?}", result),
        }
        
        // Test resolving non-local (should go to parent)
        let global_ident = Ident::from("global-var");
        let result = scope.resolve(global_ident);
        
        match result {
            Var::Global(_) => {
                // Should resolve to global
            }
            _ => panic!("Expected Global variant for non-local, got {:?}", result),
        }
    }

    #[test]
    fn test_nested_lexical_scopes() {
        let env = create_test_env();
        let outer_binding = create_test_binding(vec![("outer-var", true)]);
        let inner_binding = create_test_binding(vec![("inner-var", true)]);
        
        let outer_scope = Scope::Lexical {
            binding: outer_binding,
            parent: Box::new(Scope::Global(&env)),
        };
        
        let inner_scope = Scope::Lexical {
            binding: inner_binding,
            parent: Box::new(outer_scope),
        };
        
        // Test resolving inner variable
        let inner_result = inner_scope.resolve(Ident::from("inner-var"));
        assert!(matches!(inner_result, Var::Local(_)));
        
        // Test resolving outer variable from inner scope
        let outer_result = inner_scope.resolve(Ident::from("outer-var"));
        assert!(matches!(outer_result, Var::Local(_)));
        
        // Test resolving global variable
        let global_result = inner_scope.resolve(Ident::from("global-var"));
        assert!(matches!(global_result, Var::Global(_)));
    }

    #[test]
    fn test_variable_shadowing() {
        let env = create_test_env();
        let outer_binding = create_test_binding(vec![("x", true)]);
        let inner_binding = create_test_binding(vec![("x", true)]);
        
        let outer_scope = Scope::Lexical {
            binding: outer_binding,
            parent: Box::new(Scope::Global(&env)),
        };
        
        let inner_scope = Scope::Lexical {
            binding: inner_binding,
            parent: Box::new(outer_scope),
        };
        
        // Inner scope should shadow outer scope
        let result = inner_scope.resolve(Ident::from("x"));
        assert!(matches!(result, Var::Local(_)));
    }

    #[test]
    fn test_function_captures_from_lexical() {
        let env = create_test_env();
        let binding = create_test_binding(vec![("captured-var", true)]);
        let args = create_test_args(vec!["arg"], None, None);
        let captures = Rc::new(RefCell::new(Vec::new()));
        
        let lexical_scope = Scope::Lexical {
            binding,
            parent: Box::new(Scope::Global(&env)),
        };
        
        let function_scope = Scope::Function {
            args,
            captures: captures.clone(),
            parent: Box::new(lexical_scope),
        };
        
        // Resolve a variable that should be captured
        let result = function_scope.resolve(Ident::from("captured-var"));
        
        // Should return captured
        assert!(matches!(result, Var::Captured(_)));
        
        // Check that it was added to captures
        let captures_vec = captures.borrow();
        assert_eq!(captures_vec.len(), 1);
        assert!(matches!(captures_vec[0], Var::Local(_)));
    }

    #[test]
    fn test_function_captures_from_global() {
        let env = create_test_env();
        let args = create_test_args(vec!["arg"], None, None);
        let captures = Rc::new(RefCell::new(Vec::new()));
        
        let function_scope = Scope::Function {
            args,
            captures: captures.clone(),
            parent: Box::new(Scope::Global(&env)),
        };
        
        // Resolve a global variable that should be captured
        let result = function_scope.resolve(Ident::from("global-var"));
        
        // Should return captured
        assert!(matches!(result, Var::Captured(_)));
        
        // Check that it was added to captures
        let captures_vec = captures.borrow();
        assert_eq!(captures_vec.len(), 1);
        assert!(matches!(captures_vec[0], Var::Global(_)));
    }

    #[test]
    fn test_already_captured_variable() {
        let env = create_test_env();
        let args = create_test_args(vec!["arg"], None, None);
        let captures = Rc::new(RefCell::new(vec![Var::Global(Ident::from("already-captured").into())]));
        
        let function_scope = Scope::Function {
            args,
            captures: captures.clone(),
            parent: Box::new(Scope::Global(&env)),
        };
        
        // Resolve a variable that's already captured
        let result = function_scope.resolve(Ident::from("already-captured"));
        
        // Should return captured without adding to captures again
        assert!(matches!(result, Var::Captured(_)));
        
        // Check that captures list didn't grow
        let captures_vec = captures.borrow();
        assert_eq!(captures_vec.len(), 1);
    }

    #[test]
    fn test_complex_nested_scopes() {
        let env = create_test_env();
        
        // Create: Global -> Lexical -> Function -> Lexical
        let outer_binding = create_test_binding(vec![("outer", true)]);
        let args = create_test_args(vec!["param"], None, None);
        let captures = Rc::new(RefCell::new(Vec::new()));
        let inner_binding = create_test_binding(vec![("inner", true)]);
        
        let global_scope = Scope::Global(&env);
        
        let outer_lexical = Scope::Lexical {
            binding: outer_binding,
            parent: Box::new(global_scope),
        };
        
        let function_scope = Scope::Function {
            args,
            captures: captures.clone(),
            parent: Box::new(outer_lexical),
        };
        
        let inner_lexical = Scope::Lexical {
            binding: inner_binding,
            parent: Box::new(function_scope),
        };
        
        // Test various resolutions
        assert!(matches!(inner_lexical.resolve(Ident::from("inner")), Var::Local(_)));
        assert!(matches!(inner_lexical.resolve(Ident::from("param")), Var::Argument(_)));
        assert!(matches!(inner_lexical.resolve(Ident::from("outer")), Var::Local(_)));
        assert!(matches!(inner_lexical.resolve(Ident::from("global")), Var::Global(_)));
        
        // Check captures were recorded
        let captures_vec = captures.borrow();
        assert!(captures_vec.len() >= 1); // Should have captured 'outer' and 'global'
    }

    #[test]
    fn test_args_contains() {
        let args = Args {
            normal: vec![Ident::from("x"), Ident::from("y")],
            optional: Some(vec![Ident::from("opt1"), Ident::from("opt2")]),
            rest: Some(Ident::from("rest")),
        };
        
        // Test normal args
        assert!(args.contains(Ident::from("x")));
        assert!(args.contains(Ident::from("y")));
        
        // Test optional args
        assert!(args.contains(Ident::from("opt1")));
        assert!(args.contains(Ident::from("opt2")));
        
        // Test rest arg
        assert!(args.contains(Ident::from("rest")));
        
        // Test non-existent arg
        assert!(!args.contains(Ident::from("nonexistent")));
    }

    #[test]
    fn test_args_contains_no_optional_no_rest() {
        let args = Args {
            normal: vec![Ident::from("x")],
            optional: None,
            rest: None,
        };
        
        assert!(args.contains(Ident::from("x")));
        assert!(!args.contains(Ident::from("y")));
    }
}
