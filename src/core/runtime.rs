use super::{env::Environment, function::Function};
use crate::core::{
    cons::Cons,
    function::{FuncPtr, FunctionType},
    ident::Ident,
    symbol::Symbol,
    value::{LispValue, TaggedPtr, Value},
};
use anyhow::{anyhow, bail, Result};
use proc_macros::{defun, internal_fn};

#[defun(name = "+")]
fn add(vals: &[Value]) -> Result<Value> {
    tracing::debug!("calling add: {vals:?}");
    let mut result = 0;
    for val in vals.iter() {
        let LispValue::Int(v) = val.untag() else {
            bail!("type error")
        };
        result += v;
    }
    println!("{result}");
    Ok(result.tag())
}

#[internal_fn]
fn apply(func: &Function, args: &[Value], env: &Environment) -> Result<Value> {
    tracing::debug!("calling apply, {func:?} {args:?}");
    func.run(args, env)
}

fn funcall(func: Value, args: &[Value], env: &Environment) -> Result<Value> {
    match func.untag() {
        LispValue::Symbol(sym) => {
            let LispValue::Function(func) = env.load_symbol(sym, true)?.untag() else {
                bail!("wrong type arg")
            };
            func.run(args, env)
        }
        _ => bail!("wrong type arg")
    }
    // env
    // func.run(args, env)
}

#[internal_fn]
fn defvar(name: i64, len: usize, value: Value, env: &Environment) -> Result<Value> {
    let name: &str = unsafe {
        let ptr: *const u8 = std::mem::transmute(name);
        let slice = std::slice::from_raw_parts(ptr, len);
        str::from_utf8(slice)?
    };
    let mut cell = env.get_or_init_symbol(name.into());
    cell.value_mut().data().value = value;
    Ok(value)
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
    Err(anyhow!("failed to load captured value"))
}

#[internal_fn]
pub fn store_symbol_function(symbol: Symbol, func: Value, env: &Environment) {
    let mut data_ref = env.get_or_init_symbol(symbol);
    let cell = Cons::new(Symbol::new("function".into()).tag(), func);
    data_ref.value_mut().data().func = cell.tag();
}

#[internal_fn]
fn load_symbol_value(symbol: Symbol, load_function_cell: u64, env: &Environment) -> Result<Value> {
    tracing::debug!("loading symbol value: {} {load_function_cell}", symbol.name());
    println!("{:?}", &env.symbol_map);
    env.load_symbol(symbol, load_function_cell == 1)
}

#[internal_fn]
fn get_func_ptr(func: Value, env: &Environment) -> Result<FuncPtr> {
    match func.untag() {
        LispValue::Symbol(symbol) => {
            let func = env
                .load_symbol(symbol, true)?;
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
