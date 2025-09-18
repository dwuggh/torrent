//! Optimization framework for the Lisp compiler.
//!
//! This module provides a multi-stage optimization pipeline with:
//! - Scope analysis and variable resolution
//! - Type inference and hint propagation
//! - Dead code elimination
//! - Constant folding
//! - Function inlining
//! - Loop optimizations

// pub mod ir;
// pub mod passes;
// pub mod pipeline;
// pub mod scope;
// pub mod types;

// pub use ir::{OptimizedExpr, OptimizationInfo, ScopedExpr, TypedExpr};
// pub use passes::*;
// pub use pipeline::{OptimizationPipeline, OptimizationConfig};
// pub use scope::{ScopeAnalyzer, VariableResolver};
// pub use types::TypeInference;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum OptimizationError {
    #[error("Scope analysis error: {message}")]
    ScopeError { message: String },

    #[error("Type inference error: {message}")]
    TypeError { message: String },

    #[error("Variable resolution error: {variable} not found")]
    UnresolvedVariable { variable: String },

    #[error("Optimization pass '{pass}' failed: {reason}")]
    PassFailed { pass: String, reason: String },

    #[error("Invalid optimization configuration: {message}")]
    ConfigError { message: String },
}

pub type OptimizationResult<T> = Result<T, OptimizationError>;
