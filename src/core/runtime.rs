use super::{env::Environment, function::LispFunction};
use crate::{
    core::{
        cons::LispCons,
        error::{RuntimeError, RuntimeResult},
        function::{FuncPtr, Function, FunctionType},
        ident::Ident,
        number::LispInteger,
        object::{nil, tru, LispObject, Object, ObjectRef},
        symbol::Symbol,
        TaggedPtr,
    },
    runtime_bail,
};
type Result<T> = RuntimeResult<T>;
use proc_macros::{defun, internal_fn};

use crate::runtime_error;

// for runtime functions, we need to pay special attention to its ref counting:
// 1. in FFI, return is always i64. FFI will never change GC state. all GC resource management happens inside safe rust, and will be managed automatically by rust's rules.
// 2. in rust functions, ref counting is always automatically managed.
// 3. one should always use `LispValueRef` and `LispValueMut` for function arguments, as the macro will auto-transform a value to it, so we will not have to worry about ownership.
// 4. returning a Value is trivial, returning a `LispValueRef` or its inner types is disallowed because there's no way to determine its value for it. instead, one should use `ValueRef`, which contains a backref to the value, for this propose.

#[defun(name = "-")]
fn minus(left: Object, right: Object) -> Result<Object> {
    let ObjectRef::Int(l) = left.as_ref() else {
        runtime_bail!(WrongType, expected: "integer", actual: left.get_tag());
    };
    let ObjectRef::Int(r) = right.as_ref() else {
        runtime_bail!(WrongType, expected: "integer", actual: right.get_tag());
    };
    let result = l - r;
    Ok(LispInteger(result).tag())
}

#[defun(name = "<")]
fn smaller_than(l: i64, r: i64) -> Object {
    if l < r {
        tru()
    } else {
        nil()
    }
}

#[defun(name = "-1")]
fn minus_1(num: i64) -> Result<Object> {
    let result = num - 1;
    Ok(LispInteger(result).tag())
}

#[defun(name = "+")]
fn add(vals: &[Object]) -> Result<Object> {
    tracing::debug!("calling add: {vals:?}");
    let mut result = 0;
    for val in vals.iter() {
        let ObjectRef::Int(v) = val.as_ref() else {
            runtime_bail!(WrongType, expected: "integer", actual: val.get_tag());
        };
        result += v;
    }
    Ok(LispInteger(result).tag())
}

#[internal_fn]
fn apply(func: &Function, args: &[Object], env: &Environment) -> Result<Object> {
    tracing::debug!("calling apply, {func:?} {args:?}");
    func.run(args, env)
}

#[defun]
fn funcall(func: &Object, args: &[Object], env: &Environment) -> Result<Object> {
    tracing::debug!("calling funcall: {func:?} {:?}", args);
    match func.as_ref() {
        ObjectRef::Symbol(sym) => {
            env.load_symbol_with(sym, true, |obj| {
                let ObjectRef::Function(func) = obj.as_ref() else {
                    runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
                };
                func.run(args, env)
            })
            // val is dropped here. its refcount increased in load_symbol, so its fine.
        }
        ObjectRef::Function(func) => func.run(args, env),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    }
}

#[internal_fn]
fn defvar(name: i64, len: usize, value: Object, env: &Environment) -> Result<Symbol> {
    if name == 0 {
        runtime_bail!(InternalError, message: "null pointer passed to defvar".to_string());
    }

    let name: &str = unsafe {
        let ptr = name as *const u8;
        let slice = std::slice::from_raw_parts(ptr, len);
        std::str::from_utf8(slice)
            .map_err(|_| RuntimeError::internal_error("invalid UTF-8 in symbol name"))?
    };

    let symbol = name.into();
    tracing::debug!("calling defvar: symbol {symbol:?}, value: {value:?}");
    let cell = env.get_or_init_symbol(symbol);
    cell.get().data().value = value;
    Ok(symbol)
}

#[internal_fn]
fn store_captured(ident: Ident, value: Object, func: &mut Function) {
    let FunctionType::Lambda(func) = func.get_func_type_mut() else {
        return;
    };
    func.captures.insert(ident, value);
}

#[internal_fn]
fn load_captured(ident: Ident, func: &mut Function) -> Result<Object> {
    let Some(closure) = func.get_func_type_mut().as_closure_mut() else {
        return Err(RuntimeError::internal_error("wrong type of function"));
    };
    if let Some(v) = closure.captures.get(&ident) {
        // HACK we don't want to clone here every time the captured value is loaded
        // TODO maybe just return Result<i64>, so we will not need this hack
        return Ok(Object(v.0));
    }
    runtime_bail!(UnboundSymbol, symbol: ident.into())
}

#[internal_fn]
pub fn store_symbol_function(symbol: Symbol, func: Object, env: &Environment) {
    let data_ref = env.get_or_init_symbol(symbol);
    let symbol = Symbol::new("function".into()).tag();
    let cell = LispCons::new(symbol, func);
    data_ref.get().data().func = cell.tag();
}

#[internal_fn]
fn load_symbol_value(symbol: Symbol, load_function_cell: u64, env: &Environment) -> Result<i64> {
    tracing::debug!(
        "loading symbol value: {} {load_function_cell}",
        symbol.name()
    );
    env.load_symbol_with(symbol, load_function_cell == 1, |obj| Ok(obj.0 as i64))
}

#[internal_fn]
fn get_func_ptr(func: &Object, env: &Environment) -> Result<i64> {
    let func = match func.as_ref() {
        ObjectRef::Symbol(symbol) => {
            env.load_symbol_with(symbol, true, |func| {
                let ObjectRef::Function(func) = func.as_ref() else {
                    runtime_bail!(WrongType, expected: "function", actual: func.get_tag());
                };
                // TODO func will be dropped here
                // to prevent that, we should add the caller as argument, and add the function to the caller
                get_func_ptr_from_function(func, env)
            })
        }
        ObjectRef::Function(func) => get_func_ptr_from_function(func, env),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    }?;
    Ok(func as i64)
}

fn get_func_ptr_from_function(func: &Function, env: &Environment) -> Result<FuncPtr> {
    func.func_ptr
        .ok_or_else(|| RuntimeError::internal_error("failed to get function pointer"))
}
