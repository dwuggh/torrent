use crate::core::{
    env::{Environment, FuncCellType},
    error::{RuntimeError, RuntimeResult},
    ident::Ident,
    object::{LispObject, ObjectRef},
    parser::expr::{Call, Expr},
};

pub fn macro_expand(expr: Expr, env: &Environment) -> Expr {
    todo!()
}

pub fn macro_expand_call(call: &Call, env: &Environment) -> Option<RuntimeResult<Expr>> {
    let symbol = call.symbol.get().into();
    let result = env.load_symbol_with(symbol, Some(FuncCellType::Macro), |val| {
        let ObjectRef::Function(func) = val.as_ref() else {
            return Ok(Err(RuntimeError::WrongType {
                expected: "function",
                actual: (val.get_tag()),
            }));
        };
        let args = call.args.iter().map(|arg| {
            let obj: LispObject = arg.clone().into();
            obj.tag()
        }
        ).collect::<Vec<_>>();
        let result = func.run(&args, env);
        Ok(result)
    }).ok();

    result
}

/// Convert a LispObject to Vec<Ident> for macro expansion
/// This extracts identifiers from various LispObject types
pub fn lisp_object_to_idents(obj: LispObject) -> Result<Vec<Ident>, &'static str> {
    match obj {
        LispObject::Nil => Ok(Vec::new()),
        LispObject::Symbol(symbol) => Ok(vec![symbol.0.ident()]),
        LispObject::Cons(cons) => {
            let cons_data = cons.0.get();
            if let Some(vec) = cons_data.to_vec() {
                let mut idents = Vec::new();
                for item in vec {
                    match item.as_ref() {
                        ObjectRef::Symbol(symbol) => idents.push(symbol.ident()),
                        _ => return Err("List contains non-symbol elements"),
                    }
                }
                Ok(idents)
            } else {
                Err("Improper list cannot be converted to idents")
            }
        }
        LispObject::Vector(vector) => {
            let mut idents = Vec::new();
            for item in vector.0.get() {
                match item.as_ref() {
                    ObjectRef::Symbol(symbol) => idents.push(symbol.ident()),
                    _ => return Err("Vector contains non-symbol elements"),
                }
            }
            Ok(idents)
        }
        _ => Err("Cannot convert this type to Vec<Ident>"),
    }
}

/// Convert a single LispObject to an Ident if it's a symbol
pub fn lisp_object_to_ident(obj: LispObject) -> Result<Ident, &'static str> {
    match obj {
        LispObject::Symbol(symbol) => Ok(symbol.0.ident()),
        _ => Err("Object is not a symbol"),
    }
}

/// Convert Vec<Ident> back to a LispObject (as a list of symbols)
pub fn idents_to_lisp_object(idents: Vec<Ident>) -> LispObject {
    if idents.is_empty() {
        return LispObject::Nil;
    }

    let objects: Vec<_> = idents
        .into_iter()
        .map(|ident| {
            let symbol = crate::core::symbol::Symbol::from(ident);
            LispObject::Symbol(crate::core::symbol::LispSymbol(symbol)).tag()
        })
        .collect();

    match crate::core::cons::Cons::from_vec(objects) {
        Some(cons) => LispObject::Cons(crate::core::cons::LispCons(crate::gc::Gc::new(cons))),
        None => LispObject::Nil,
    }
}
