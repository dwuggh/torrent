use crate::core::{
    env::{Environment, FuncCellType},
    error::{RuntimeError, RuntimeResult},
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
