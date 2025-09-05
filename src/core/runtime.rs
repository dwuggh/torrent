
use crate::core::value::Value;
use super::{env::Environment, function::Function};
use anyhow::Result;
use proc_macros::defun;



#[defun]
fn apply(func: &Function, args: &[Value], env: &mut Environment) -> Result<Value> {
    func.run(args, env)
}