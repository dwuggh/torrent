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
    parent: &'a CompileScope<'a>,
}

#[derive(Clone)]
pub enum CompileScope<'a> {
    Global(GlobalScope<'a>),
    Frame(FrameScope<'a>),
}

impl<'a> FrameScope<'a> {
    pub fn new(vars: HashMap<Symbol, Variable>, parent: &'a CompileScope<'a>) -> Self {
        FrameScope {
            slots: ParamSlots::new(vars),
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
    ) -> Option<Value> {
        self.load_symbol_inner(symbol, load_function_cell, builder, true)
    }

    fn load_symbol_inner(
        &self,
        symbol: Symbol,
        load_function_cell: bool,
        builder: &mut FunctionBuilder,
        same_func_scope: bool,
    ) -> Option<Value> {
        match self {
            CompileScope::Global(_) => {
                let val = Environment::default().load_symbol(symbol, load_function_cell)?;
                Some(builder.ins().iconst(types::I64, val.0 as i64))
            }
            CompileScope::Frame(frame) => same_func_scope
                .then(|| {
                    let var = frame.slots.get(symbol)?;
                    Some(builder.use_var(var))
                })
                .flatten()
                .or(frame.parent.load_symbol_inner(
                    symbol,
                    load_function_cell,
                    builder,
                    false,
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