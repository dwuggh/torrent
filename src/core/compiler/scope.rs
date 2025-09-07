use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use crate::core::ident::Ident;
use crate::core::value::Value as RuntimeValue;
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
    pub slots: ParamSlots,
    pub lexical_binds: Option<RefCell<HashMap<Ident, HashSet<RuntimeValue>>>>,
    is_func: bool,
    parent: &'a CompileScope<'a>,
}

#[derive(Clone)]
pub enum CompileScope<'a> {
    Global(GlobalScope<'a>),
    Frame(FrameScope<'a>),
}

impl<'a> FrameScope<'a> {
    pub fn new(
        vars: HashMap<Ident, Variable>,
        parent: &'a CompileScope<'a>,
        lexical_binding: bool,
        is_func: bool,
    ) -> Self {
        let lexical_binds = lexical_binding.then_some(Default::default());
        FrameScope {
            slots: ParamSlots::new(vars),
            lexical_binds,
            is_func,
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
    // pub fn get_root(&self) -> &Environment {
    //     match self {
    //         CompileScope::Global(root) => root.env,
    //         CompileScope::Frame(frame) => frame.parent.get_root(),
    //     }
    // }

}

#[derive(Debug, Clone)]
pub struct ParamSlots {
    slots: HashMap<Ident, Variable>,
}

impl ParamSlots {
    pub fn new(slots: HashMap<Ident, Variable>) -> Self {
        Self { slots }
    }

    pub fn get(&self, ident: Ident) -> Option<Variable> {
        self.slots.get(&ident).copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Val {
    Value(Value),
    Ident(Ident),
}
