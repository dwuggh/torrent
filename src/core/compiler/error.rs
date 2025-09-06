use thiserror::Error;

use crate::{ast::Node, core::symbol::Symbol};
use cranelift_module::ModuleError;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Invalid function argument format")]
    InvalidArgFormat,

    #[error("Let requires bindings and body")]
    LetMissingBindingsAndBody,

    #[error("Let bindings must be a list")]
    LetBindingsNotList,

    #[error("Let binding must be symbol or (symbol value)")]
    LetInvalidBindingFormat,

    #[error("Let binding symbol must be an identifier")]
    LetBindingNotSymbol,

    #[error("Invalid s-expression head. Expected symbol, got: {0:?}")]
    InvalidSexpHead(Node),

    #[error("Symbol not found: {0:?}")]
    SymbolNotFound(Symbol),

    #[error("Cranelift module error: {0}")]
    ModuleError(#[from] ModuleError),

    #[error("Error from dependent component: {0}")]
    Other(#[from] anyhow::Error),
}

pub type CodegenResult<T = ()> = Result<T, CodegenError>;
