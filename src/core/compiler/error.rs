use thiserror::Error;

use crate::{ast::Node, core::symbol::Symbol};
use cranelift_module::ModuleError;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Invalid function argument format")]
    InvalidArgFormat,

    #[error("Invalid let syntax: {0}")]
    InvalidLetSyntax(String),

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
