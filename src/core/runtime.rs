use super::{env::Environment, function::Function};
use crate::core::{
    function::FunctionType, ident::Ident, symbol::Symbol, value::{TaggedPtr, Value}
};
use anyhow::{anyhow, Result};
use proc_macros::defun;

#[defun]
fn apply(func: &Function, args: &[Value], env: &mut Environment) -> Result<Value> {
    func.run(args, env)
}

#[defun(is_lisp_subr = false)]
fn store_captured(ident: Ident, value: Value, func: &Function) {
    let FunctionType::Lambda(func) = func.get_func_type_mut() else {
        return;
    };
    func.captures.insert(ident, value.clone());
}

#[defun(is_lisp_subr = false)]
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

#[defun]
fn store_symbol_function(symbol: Symbol, func: Value) {
    let mut data_ref = symbol.get_or_init();
    data_ref.value_mut().data().func = Some(func)
}

fn get_func_ptr(func: &Function) {}