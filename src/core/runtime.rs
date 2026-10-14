use super::env::{DynamicEnv, Environment};
use crate::{
    core::{
        cons::LispCons,
        env::FuncCellType,
        error::{RuntimeError, RuntimeResult},
        function::{ARGV_ABI_MARKER, Function, FunctionKind},
        indirect::Indirect,
        number::LispInteger,
        object::{Object, ObjectRef, nil, tru},
        symbol::{LispSymbol, Symbol},
        tag::Tag,
    },
    gc::{GcContext, Rooted, RootedSlice},
    runtime_bail,
};
type Result<T> = RuntimeResult<T>;
use proc_macros::{defun, internal_fn};

use crate::runtime_error;

#[defun(name = "-")]
fn minus(left: &Object, right: &Object) -> Result<Object> {
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
fn add(vals: &RootedSlice, ctx: &mut GcContext) -> Result<Object> {
    tracing::debug!("calling add: {vals:?}");
    let mut result = 0;
    for val in vals.iter() {
        let val = val.get(ctx);
        let ObjectRef::Int(v) = val.as_ref() else {
            runtime_bail!(WrongType, expected: "integer", actual: val.get_tag());
        };
        result += v;
    }
    Ok(LispInteger(result).tag())
}

#[internal_fn]
fn apply(
    func: &Rooted,
    args: &RootedSlice,
    env: &Environment,
    ctx: &mut GcContext,
) -> Result<Object> {
    tracing::debug!("calling apply, {func:?} {args:?}");
    call_lisp(func, args, env, ctx)
}

#[defun]
fn funcall(
    func: &Rooted,
    args: &RootedSlice,
    env: &Environment,
    ctx: &mut GcContext,
) -> Result<Object> {
    tracing::debug!("calling funcall: {func:?} {:?}", args);
    call_lisp(func, args, env, ctx)
}

pub fn call_lisp(
    func: &Rooted,
    args: &RootedSlice,
    env: &Environment,
    ctx: &mut GcContext,
) -> Result<Object> {
    let func_obj = *func.get(ctx);

    match func_obj.as_ref() {
        ObjectRef::Symbol(sym) => {
            let guard = env.load_symbol_guard(sym, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(_) = obj.as_ref() else {
                runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
            };
            let rooted_func = ctx.root(obj.clone());
            call_function_rooted(&rooted_func, args, env, ctx)
            // val is dropped here. its refcount increased in load_symbol, so its fine.
        }
        ObjectRef::Function(_) => call_function_rooted(func, args, env, ctx),
        _ => runtime_bail!(WrongType, expected: "function", actual: func_obj.get_tag()),
    }
}

fn call_function_rooted(
    func: &Rooted,
    args: &RootedSlice,
    env: &Environment,
    ctx: &mut GcContext,
) -> Result<Object> {
    let func_obj = *func.get(ctx);
    let ObjectRef::Function(func) = func_obj.as_ref() else {
        runtime_bail!(WrongType, expected: "function", actual: func_obj.get_tag());
    };
    func.run_rooted(args, env, ctx)
}

#[internal_fn]
fn load_func(symbol_or_func: &Object, env: &Environment) -> Result<Object> {
    let symbol_or_func = *symbol_or_func;
    match symbol_or_func.as_ref() {
        ObjectRef::Symbol(sym) => {
            let guard = env.load_symbol_guard(sym, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(_) = obj.as_ref() else {
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
fn defvar(
    name: i64,
    len: usize,
    value: &Rooted,
    env: &Environment,
    ctx: &mut GcContext,
) -> Result<Object> {
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
    let value = *value.get(ctx);
    tracing::debug!("calling defvar: symbol {symbol:?}, value: {value:?}");
    let mut symbol_map = env.symbol_map();
    let symbol = symbol_map.as_mut().set_value(symbol, value);
    Ok(symbol.tag())
}

#[internal_fn]
fn create_indirect_object(object: &Rooted, ctx: &mut GcContext) -> Object {
    Indirect::new_object_rooted(object, ctx)
}

#[internal_fn]
fn load_indirect(object: &Object) -> Result<Object> {
    let object = *object;
    if !Indirect::is_object(object.clone()) {
        return Err(RuntimeError::internal_error(
            "object is not an indirect cell",
        ));
    }
    Ok(unsafe { Indirect::from_object(object) }.load())
}

#[internal_fn]
fn store_indirect(object: &Object, value: &Object) -> Result<Object> {
    let object = *object;
    let value = *value;
    if !Indirect::is_object(object.clone()) {
        return Err(RuntimeError::internal_error(
            "object is not an indirect cell",
        ));
    }
    unsafe { Indirect::from_object_mut(object) }.store(value.clone());
    Ok(value)
}

#[internal_fn]
fn store_lexical(index: usize, object: &Object, func: &Object) -> Result<Object> {
    tracing::debug!("calling store_lexical");
    let object = *object;
    let func_obj = *func;
    let func: &mut Function = (&func_obj)
        .try_into()
        .map_err(|_| RuntimeError::wrong_type("function", func_obj.get_tag()))?;
    func.store_lexical(index, object.clone())?;
    Ok(object)
}

#[internal_fn]
fn load_lexical(index: usize, func: &Object) -> Result<Object> {
    tracing::debug!("calling load_lexical");
    let func_obj = *func;
    let ObjectRef::Function(func) = func_obj.as_ref() else {
        runtime_bail!(WrongType, expected: "function", actual: func_obj.get_tag());
    };
    func.load_lexical(index)
}

#[internal_fn]
fn get_lexical_bindings(func: &Object) -> Result<Object> {
    match func.as_ref() {
        ObjectRef::Function(_) => Ok(*func),
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    }
}

#[internal_fn]
fn create_closure(id: u64, code: i64, signature: u64, count: usize, ctx: &mut GcContext) -> Object {
    use crate::core::function::{FunctionInit, FunctionSignature, LispFunction};
    use cranelift::prelude::EntityRef;
    LispFunction::new(
        FunctionInit {
            id: cranelift_module::FuncId::new(id as usize),
            code: code as *const u8,
            signature: FunctionSignature::new(
                signature as u8,
                (signature >> 8) as u8,
                (signature >> 16) != 0,
            ),
            kind: FunctionKind::Jit,
            binding_len: count,
        },
        ctx,
    )
    .tag()
}

#[internal_fn]
fn load_capture(index: usize, func: &Object) -> Result<Object> {
    let ObjectRef::Function(function) = func.as_ref() else {
        runtime_bail!(WrongType, expected: "function", actual: func.get_tag());
    };
    function.binding(index)
}

#[internal_fn]
fn init_capture(index: usize, value: &Object, func: &Object) -> Result<Object> {
    let function: &mut Function = func
        .try_into()
        .map_err(|_| RuntimeError::wrong_type("function", func.get_tag()))?;
    function.set_binding(index, *value)?;
    Ok(*value)
}

#[internal_fn]
pub fn store_symbol_function(
    symbol: Symbol,
    func: &Rooted,
    env: &Environment,
    ctx: &mut GcContext,
) {
    tracing::debug!("storing function {func:?} to symbol {}", symbol.name());
    let marker = LispSymbol::from("function").tag();
    let marker = ctx.root(marker);
    let cell = LispCons::new_rooted(&marker, func, ctx);
    let mut symbol_map = env.symbol_map();
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
    let symbol_map = env.symbol_map();
    let Some(cell) = symbol_map.as_ref().get_symbol_cell_ref(symbol) else {
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
    let func = *func;
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
    let func = *func;
    let is_native = match func.as_ref() {
        ObjectRef::Symbol(symbol) => {
            let guard = env.load_symbol_guard(symbol, Some(FuncCellType::Function))?;
            let obj = guard.as_ref();
            let ObjectRef::Function(func) = obj.as_ref() else {
                runtime_bail!(WrongType, expected: "function", actual: obj.get_tag());
            };
            func.kind() == FunctionKind::Native
        }
        ObjectRef::Function(func) => func.kind() == FunctionKind::Native,
        _ => runtime_bail!(WrongType, expected: "function", actual: func.get_tag()),
    };

    Ok(is_native as i64)
}

#[internal_fn]
fn check_function_args(func: &Object, argc: usize) -> i64 {
    let func = *func;
    let ObjectRef::Function(func) = func.as_ref() else {
        return -1;
    };
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
fn signal_wrong_number_of_args(func: &Object, argc: usize) -> Result<Object> {
    let func_obj = *func;
    let ObjectRef::Function(func) = func_obj.as_ref() else {
        runtime_bail!(WrongType, expected: "function", actual: func_obj.get_tag());
    };
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
fn bind_special(symbol: Symbol, value: &Object, env: &Environment) -> Result<()> {
    if env.is_special(symbol) {
        env.push_special_symbol(symbol, *value)?;
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
fn collect_rest_args(arg_ptr: i64, argc: i64, ctx: &mut GcContext) -> Object {
    let argc: usize = argc as usize;
    let arg_ptr = arg_ptr as *const Object;
    let args = unsafe { std::slice::from_raw_parts(arg_ptr, argc) };
    let roots = ctx.root_slice(args);
    LispCons::list_from_rooted_slice(&roots, ctx)
}

#[internal_fn]
fn is_special(symbol: Symbol, env: &Environment) -> i64 {
    env.is_special(symbol) as i64
}

fn get_func_ptr_from_function(func: &Function, _env: &Environment) -> Result<*const u8> {
    func.get_func_ptr()
        .ok_or_else(|| RuntimeError::internal_error("failed to get function pointer"))
}

#[internal_fn]
fn make_float(bits: u64, ctx: &mut GcContext) -> Object {
    crate::core::number::LispFloat::new(f64::from_bits(bits), ctx).tag()
}

#[internal_fn]
fn make_string(ptr: i64, len: usize, ctx: &mut GcContext) -> Object {
    // Generated code supplies a valid UTF-8 byte array for this call.
    let bytes = unsafe { std::slice::from_raw_parts(ptr as *const u8, len) };
    let string = std::str::from_utf8(bytes)
        .expect("invalid generated string")
        .to_owned();
    crate::core::string::LispStr::new(string, ctx).tag()
}

#[internal_fn]
fn make_cons(car: &Rooted, cdr: &Rooted, ctx: &mut GcContext) -> Object {
    LispCons::new_rooted(car, cdr, ctx).tag()
}

#[internal_fn]
fn make_vector(values: &RootedSlice, ctx: &mut GcContext) -> Object {
    crate::core::vector::LispVector(ctx.alloc::<crate::core::vector::Vector>(values)).tag()
}

#[internal_fn]
fn set_symbol_value(symbol: Symbol, value: &Object, env: &Environment) -> Object {
    env.symbol_map().as_mut().set_value(symbol, *value);
    *value
}
