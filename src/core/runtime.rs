use super::{env::Environment, function::Function};
use crate::core::{function::FunctionType, symbol::Symbol, value::Value};
use anyhow::{anyhow, Result};
use proc_macros::defun;

#[defun]
fn apply(func: &Function, args: &[Value], env: &mut Environment) -> Result<Value> {
    func.run(args, env)
}

#[defun]
fn __store_lexical(symbol: Symbol, value: Value, func: &Function) {
    let FunctionType::Lambda(func) = func.get_func_type_mut() else {
        return;
    };
    func.captures.insert(symbol, value.clone());
}

#[defun]
fn __load_captured(symbol: Symbol, func: &Function) -> Result<Value> {
    func.get_func_type_mut()
        .as_closure_mut()
        .and_then(|closure| closure.captures.get(&symbol))
        .copied()
        .ok_or(anyhow!("failed to load captured value"))
}

#[defun]
fn store_symbol_function(symbol: Symbol, func: Value) {
    let mut data_ref = symbol.get_or_init();
    data_ref.value_mut().data().func = Some(func)
}