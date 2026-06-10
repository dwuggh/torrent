use super::env::{DynamicEnv, Environment};
use crate::{
    core::{
        cons::LispCons,
        env::FuncCellType,
        error::{RuntimeError, RuntimeResult},
        function::{ARGV_ABI_MARKER, Function, FunctionType},
        indirect::Indirect,
        number::LispInteger,
        object::{Object, ObjectRef, nil, tru},
        symbol::{LispSymbol, Symbol},
        tag::Tag,
    },
    runtime_bail,
};
type Result<T> = RuntimeResult<T>;
use proc_macros::{defun, internal_fn};

use crate::runtime_error;

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
    if l < r { tru() } else { nil() }
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
            let guard = env.load_symbol_guard(sym, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(func) = obj.as_ref() else {
                runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
            };
            func.run(args, env)
            // val is dropped here. its refcount increased in load_symbol, so its fine.
        }
        ObjectRef::Function(func) => func.run(args, env),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    }
}

#[internal_fn]
fn load_func(symbol_or_func: Object, env: &Environment) -> Result<Object> {
    match symbol_or_func.as_ref() {
        ObjectRef::Symbol(sym) => {
            let guard = env.load_symbol_guard(sym, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(func) = obj.as_ref() else {
                runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
            };
            return Ok(obj.clone());
            // val is dropped here. its refcount increased in load_symbol, so its fine.
        }
        ObjectRef::Function(_) => Ok(symbol_or_func),
        _ => runtime_bail!(WrongType, expected: "function", actual: symbol_or_func.get_tag()),
    }
}

#[internal_fn]
fn defvar(name: i64, len: usize, value: Object, env: &Environment) -> Result<Object> {
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
    let mut symbol_map = env.symbol_map.clone();
    let symbol = symbol_map.as_mut().set_value(symbol, value);
    Ok(symbol.tag())
}

#[internal_fn]
fn create_indirect_object(object: Object) -> Object {
    Indirect::new_object(object)
}

#[internal_fn]
fn load_indirect(object: Object) -> Result<Object> {
    if !Indirect::is_object(object.clone()) {
        return Err(RuntimeError::internal_error(
            "object is not an indirect cell",
        ));
    }
    Ok(unsafe { Indirect::from_object(object) }.load())
}

#[internal_fn]
fn store_indirect(object: Object, value: Object) -> Result<Object> {
    if !Indirect::is_object(object.clone()) {
        return Err(RuntimeError::internal_error(
            "object is not an indirect cell",
        ));
    }
    unsafe { Indirect::from_object_mut(object) }.store(value.clone());
    Ok(value)
}

#[internal_fn]
fn store_lexical(index: usize, object: Object, func: &mut Function) -> Result<Object> {
    tracing::debug!("calling store_lexical");
    func.store_lexical(index, object.clone())?;
    Ok(object)
}

#[internal_fn]
fn load_lexical(index: usize, func: &Function) -> Result<Object> {
    tracing::debug!("calling load_lexical");
    func.load_lexical(index)
}

#[internal_fn]
fn get_lexical_bindings(func: &Object) -> Result<i64> {
    match func.as_ref() {
        ObjectRef::Function(func) => Ok(func.lexical_ptr() as i64),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    }
}

#[internal_fn]
pub fn store_symbol_function(symbol: Symbol, func: Object, env: &Environment) {
    tracing::debug!("storing function {func:?} to symbol {}", symbol.name());
    let marker = LispSymbol::from("function").tag();
    let cell = LispCons::new(marker, func);
    let mut symbol_map = env.symbol_map.clone();
    symbol_map.as_mut().set_func(symbol, cell.tag());
}

#[internal_fn]
fn load_symbol_value(
    symbol: Symbol,
    load_function_cell: u64,
    env: &Environment,
    dynamic: i64,
) -> Result<i64> {
    tracing::debug!(
        "loading symbol value: {} {load_function_cell}",
        symbol.name()
    );
    let ty = FuncCellType::from_num(load_function_cell);
    let dynamic = if dynamic == 0 {
        None
    } else {
        unsafe { (dynamic as *const DynamicEnv).as_ref() }
    };
    let guard = env.load_symbol_guard_with_dynamic(symbol, ty, dynamic)?;
    Ok(guard.raw_i64())
}

#[internal_fn]
fn resolve_special_symbol_index(symbol: Symbol, env: &Environment) -> Result<i64> {
    let Some(cell) = env.symbol_map.as_ref().get_symbol_cell_ref(symbol) else {
        return Ok(-1);
    };

    if !cell.special {
        return Ok(-1);
    }

    Ok(cell.name.data() as i64)
}

#[internal_fn]
fn get_func_ptr(func: &Object, env: &Environment) -> Result<i64> {
    tracing::debug!("calling get_func_ptr");
    let func = match func.as_ref() {
        ObjectRef::Symbol(symbol) => {
            let guard = env.load_symbol_guard(symbol, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(func) = obj.as_ref() else {
                runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
            };
            get_func_ptr_from_function(func, env)
        }
        ObjectRef::Function(func) => get_func_ptr_from_function(func, env),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    }?;
    Ok(func as i64)
}

#[internal_fn]
fn is_native_function(func: &Object, env: &Environment) -> Result<i64> {
    let is_native = match func.as_ref() {
        ObjectRef::Symbol(symbol) => {
            let guard = env.load_symbol_guard(symbol, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(func) = obj.as_ref() else {
                runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
            };
            matches!(&func.func_type, FunctionType::Subr(_))
        }
        ObjectRef::Function(func) => matches!(&func.func_type, FunctionType::Subr(_)),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    };

    Ok(is_native as i64)
}

#[internal_fn]
fn check_function_args(func: &Function, argc: usize) -> i64 {
    let (is_valid, fixed_param_count, use_argv_abi) = func.check_args(argc);
    if !is_valid {
        return -1;
    }

    if use_argv_abi {
        ARGV_ABI_MARKER
    } else {
        fixed_param_count as i64
    }
}

#[internal_fn]
fn signal_wrong_number_of_args(func: &Function, argc: usize) -> Result<Object> {
    // Get the function signature to determine expected argument count
    let signature = &func.signature;
    let min_args = signature.normal as usize;
    let max_args = if signature.rest {
        usize::MAX
    } else {
        (signature.normal + signature.optional) as usize
    };

    let expected_msg = if signature.rest {
        min_args
    } else if signature.optional > 0 {
        max_args
    } else {
        min_args
    };

    runtime_bail!(WrongNumberOfArgs,
        expected: expected_msg,
        actual: argc
    );
}

#[internal_fn]
fn bind_special(symbol: Symbol, value: Object, env: &Environment) -> Result<()> {
    if env.is_special(symbol) {
        env.push_special_symbol(symbol, value)?;
    }
    Ok(())
}

#[internal_fn]
fn mark_bind(env: &Environment) {
    // println!("mark bind");
    env.push_spec_stack(super::env::SpecStackItem::LexicalMark);
}

#[internal_fn]
fn unbind_special(env: &Environment) -> Result<()> {
    env.pop_lexical()
}

#[internal_fn]
fn collect_rest_args(arg_ptr: i64, argc: i64) -> Object {
    let argc: usize = argc as usize;
    let arg_ptr = arg_ptr as *const i64;
    let args = unsafe { std::slice::from_raw_parts(arg_ptr, argc) };
    let iter = args.into_iter().map(|val| Object(*val as u64));
    let cons = LispCons::from_iter(iter).unwrap().tag();
    cons
}

#[internal_fn]
fn is_special(symbol: Symbol, env: &Environment) -> i64 {
    env.is_special(symbol) as i64
}

fn get_func_ptr_from_function(func: &Function, env: &Environment) -> Result<*const u8> {
    func.get_func_ptr()
        .ok_or_else(|| RuntimeError::internal_error("failed to get function pointer"))
}
