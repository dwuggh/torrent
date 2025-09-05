use std::collections::HashMap;

use cranelift::prelude::*;

use crate::core::{env::Environment, symbol::Symbol};

#[derive(Clone, Copy)]
pub struct GlobalScope<'a> {
    env: &'a Environment,
}

impl<'a> GlobalScope<'a> {
    pub fn new(env: &'a Environment) -> Self {
        Self { env }
    }
}

#[derive(Clone)]
pub struct FrameScope<'a> {
    slots: ParamSlots,
    lexical_binding: bool,
    parent: &'a CompileScope<'a>,
}

#[derive(Clone)]
pub enum CompileScope<'a> {
    Global(GlobalScope<'a>),
    Frame(FrameScope<'a>),
}

impl<'a> FrameScope<'a> {
    pub fn new(
        vars: HashMap<Symbol, Variable>,
        parent: &'a CompileScope<'a>,
        lexical_binding: bool,
    ) -> Self {
        FrameScope {
            slots: ParamSlots::new(vars),
            lexical_binding,
            parent,
        }
    }
}

impl<'a> From<FrameScope<'a>> for CompileScope<'a> {
    fn from(value: FrameScope<'a>) -> Self {
        CompileScope::Frame(value)
    }
}

impl CompileScope<'_> {
    pub fn get_root(&self) -> &Environment {
        match self {
            CompileScope::Global(root) => root.env,
            CompileScope::Frame(frame) => frame.parent.get_root(),
        }
    }

    pub fn load_symbol(
        &self,
        symbol: Symbol,
        load_function_cell: bool,
        builder: &mut FunctionBuilder,
    ) -> Option<Val> {
        let mut same_func_scope = true;
        self.load_symbol_inner(symbol, load_function_cell, builder, &mut same_func_scope)
            .map(|value| {
                if same_func_scope {
                    Val::Value(value)
                } else {
                    Val::Symbol(symbol)
                }
            })
    }

    fn load_symbol_inner(
        &self,
        symbol: Symbol,
        load_function_cell: bool,
        builder: &mut FunctionBuilder,
        same_func_scope: &mut bool,
    ) -> Option<Value> {
        match self {
            CompileScope::Global(_) => {
                let val = Environment::default().load_symbol(symbol, load_function_cell)?;
                Some(builder.ins().iconst(types::I64, val.0 as i64))
            }
            CompileScope::Frame(frame) => (*same_func_scope || frame.lexical_binding)
                .then(|| {
                    let var = frame.slots.get(symbol)?;
                    Some(builder.use_var(var))
                })
                .flatten()
                .or(frame.parent.load_symbol_inner(
                    symbol,
                    load_function_cell,
                    builder,
                    same_func_scope,
                )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParamSlots {
    slots: HashMap<Symbol, Variable>,
}

impl ParamSlots {
    pub fn new(slots: HashMap<Symbol, Variable>) -> Self {
        Self { slots }
    }

    pub fn get(&self, symbol: Symbol) -> Option<Variable> {
        self.slots.get(&symbol).copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Val {
    Value(Value),
    Symbol(Symbol),
}
