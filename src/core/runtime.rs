use super::{env::Environment, function::Function};
use crate::core::{
    cons::Cons,
    function::{FuncPtr, FunctionType},
    ident::Ident,
    symbol::Symbol,
    value::{LispValue, TaggedPtr, Value},
};
use anyhow::{anyhow, Result};
use proc_macros::{defun, internal_fn};

#[defun]
fn apply(func: &Function, args: &[Value], env: &Environment) -> Result<Value> {
    func.run(args, env)
}

#[internal_fn]
fn store_captured(ident: Ident, value: Value, func: &Function) {
    let FunctionType::Lambda(func) = func.get_func_type_mut() else {
        return;
    };
    func.captures.insert(ident, value);
}

#[internal_fn]
fn load_captured(ident: Ident, func: &Function) -> Result<Value> {
    let Some(closure) = func.get_func_type_mut().as_closure_mut() else {
        return Err(anyhow!("not a closure"));
    };
    if let Some(v) = closure.captures.get(&ident) {
        return Ok(*v);
    }
    // if let Some(cell_ref) = ident.get() {
    //     if let Some(v) = cell_ref.data().value.clone() {
    //         closure.captures.insert(key, v.clone());
    //         return Ok(v);
    //     }
    // }
    Err(anyhow!("failed to load captured value"))
}

#[internal_fn]
fn store_symbol_function(symbol: Symbol, func: Value, env: &Environment) {
    let mut data_ref = env.get_or_init_symbol(symbol);
    let cell = Cons::new(Symbol::new("function".into()).tag(), func);
    data_ref.value_mut().data().func = cell.tag();
}

#[internal_fn]
fn load_symbol_value(
    symbol: Symbol,
    load_function_cell: i64,
    env: &Environment,
) -> Option<Value> {
    let mut data_ref = env.get_or_init_symbol(symbol);
    let data = data_ref.value_mut().data();
    if load_function_cell == 1 {
        let LispValue::Cons(cons) = data.func.untag() else {
            return None;
        };
        let LispValue::Symbol(marker) = cons.car().untag() else {
            return None;
        };
        match marker.name() {
            "function" => Some(cons.cdr()),
            _ => return None,
        }
    } else {
        return Some(data.value)
    }
}

#[internal_fn]
fn get_func_ptr(func: Value, env: &Environment) -> Result<FuncPtr> {
    match func.untag() {
        LispValue::Symbol(symbol) => {
            let func = env
                .load_symbol(symbol, true)
                .ok_or(anyhow::anyhow!("symbol's function def is void"))?;
            let LispValue::Function(func) = func.untag() else {
                anyhow::bail!("wrong type")
            };
            // TODO assert func is function
            get_func_ptr_from_function(func, env)
        }
        LispValue::Function(func) => get_func_ptr_from_function(func, env),

        _ => anyhow::bail!("wrong type argument"),
    }
}

fn get_func_ptr_from_function(func: Function, env: &Environment) -> Result<FuncPtr> {
    func.get_func_ptr()
        .ok_or(anyhow::anyhow!("failed to get pointer"))
}
