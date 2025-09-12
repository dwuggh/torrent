use thiserror::Error;

use crate::core::{
    ident::Ident,
    symbol::Symbol,
    object::{LispType, Object},
};

/// Runtime errors that can occur during Lisp execution
#[derive(Debug, Error)]
pub enum RuntimeError {
    // Type errors
    #[error("Wrong type argument: expected {expected}, got {actual}")]
    WrongType {
        expected: &'static str,
        actual: LispType,
    },

    #[error("Wrong number of arguments: expected {expected}, got {actual}")]
    WrongNumberOfArgs { expected: usize, actual: usize },

    #[error("Wrong number of arguments: expected at least {min}, got {actual}")]
    TooFewArgs { min: usize, actual: usize },

    #[error("Wrong number of arguments: expected at most {max}, got {actual}")]
    TooManyArgs { max: usize, actual: usize },

    #[error("Wrong number of arguments: expected {min}-{max}, got {actual}")]
    WrongArgCount {
        min: usize,
        max: usize,
        actual: usize,
    },

    // Symbol and variable errors
    #[error("Symbol's value as variable is void: {symbol}")]
    VoidVariable { symbol: Symbol },

    #[error("Symbol's function definition is void: {symbol}")]
    VoidFunction { symbol: Symbol },

    #[error("Symbol is not bound: {symbol}")]
    UnboundSymbol { symbol: Symbol },

    #[error("Cannot set constant symbol: {symbol}")]
    SettingConstant { symbol: Symbol },

    // Function call errors
    #[error("Invalid function: {value:?}")]
    InvalidFunction { value: Object },

    #[error("Function {name} is not defined")]
    UndefinedFunction { name: Ident },

    #[error("Circular function definition: {name}")]
    CircularFunction { name: Ident },

    // Arithmetic errors
    #[error("Division by zero")]
    DivisionByZero,

    #[error("Arithmetic overflow")]
    ArithmeticOverflow,

    #[error("Invalid arithmetic operation on {operand_type}")]
    InvalidArithmetic { operand_type: LispType },

    // List and sequence errors
    #[error("Wrong type argument: {value:?} is not a list")]
    NotAList { value: Object },

    #[error("Wrong type argument: {value:?} is not a sequence")]
    NotASequence { value: Object },

    #[error("List index out of bounds: {index}")]
    IndexOutOfBounds { index: usize },

    #[error("Cannot take car of {value:?}")]
    CannotTakeCar { value: Object },

    #[error("Cannot take cdr of {value:?}")]
    CannotTakeCdr { value: Object },

    // String errors
    #[error("String index out of bounds: {index} in string of length {length}")]
    StringIndexOutOfBounds { index: usize, length: usize },

    #[error("Invalid string operation on {value:?}")]
    InvalidStringOperation { value: Object },

    // Vector errors
    #[error("Vector index out of bounds: {index} in vector of length {length}")]
    VectorIndexOutOfBounds { index: usize, length: usize },

    #[error("Wrong type argument: {value:?} is not a vector")]
    NotAVector { value: Object },

    // Control flow errors
    #[error("No catch for tag: {tag:?}")]
    NoCatch { tag: Object },

    #[error("Invalid throw without catch")]
    InvalidThrow,

    #[error("Return used outside of function")]
    ReturnOutsideFunction,

    #[error("Break used outside of loop")]
    BreakOutsideLoop,

    #[error("Continue used outside of loop")]
    ContinueOutsideLoop,

    // Condition system errors
    #[error("Unhandled error: {condition}")]
    UnhandledError { condition: String },

    #[error("Invalid error condition: {value:?}")]
    InvalidCondition { value: Object },

    #[error("Signal: {signal} with data: {data:?}")]
    Signal { signal: Symbol, data: Object },

    // I/O errors
    #[error("File not found: {filename}")]
    FileNotFound { filename: String },

    #[error("Permission denied: {filename}")]
    PermissionDenied { filename: String },

    #[error("I/O error: {message}")]
    IoError { message: String },

    #[error("End of file reached")]
    EndOfFile,

    // Buffer and editing errors
    #[error("No such buffer: {buffer_name}")]
    NoSuchBuffer { buffer_name: String },

    #[error("Buffer is read-only: {buffer_name}")]
    ReadOnlyBuffer { buffer_name: String },

    #[error("Point out of bounds: {point} in buffer of size {size}")]
    PointOutOfBounds { point: usize, size: usize },

    #[error("Invalid mark")]
    InvalidMark,

    // Syntax and parsing errors
    #[error("Invalid syntax: {message}")]
    InvalidSyntax { message: String },

    #[error("Malformed special form: {form}")]
    MalformedSpecialForm { form: String },

    #[error("Invalid lambda list: {lambda_list:?}")]
    InvalidLambdaList { lambda_list: Object },

    // Memory and resource errors
    #[error("Out of memory")]
    OutOfMemory,

    #[error("Stack overflow")]
    StackOverflow,

    #[error("Maximum recursion depth exceeded")]
    RecursionLimit,

    #[error("Resource limit exceeded: {resource}")]
    ResourceLimit { resource: String },

    // Macro errors
    #[error("Macro expansion error: {message}")]
    MacroExpansionError { message: String },

    #[error("Invalid macro definition")]
    InvalidMacroDefinition,

    // Interactive and command errors
    #[error("Command not found: {command}")]
    CommandNotFound { command: String },

    #[error("Invalid interactive specification: {spec}")]
    InvalidInteractiveSpec { spec: String },

    #[error("User quit")]
    UserQuit,

    #[error("User error: {message}")]
    UserError { message: String },

    // Generic errors
    #[error("Internal error: {message}")]
    InternalError { message: String },

    #[error("Not implemented: {feature}")]
    NotImplemented { feature: String },

    #[error("Invalid operation: {operation}")]
    InvalidOperation { operation: String },

    #[error("Error in {context}: {source}")]
    ContextualError {
        context: String,
        #[source]
        source: Box<RuntimeError>,
    },
}

impl RuntimeError {
    /// Create a wrong type error
    pub fn wrong_type(expected: &'static str, actual: LispType) -> Self {
        Self::WrongType { expected, actual }
    }

    /// Create a wrong number of arguments error
    pub fn wrong_arg_count(expected: usize, actual: usize) -> Self {
        Self::WrongNumberOfArgs { expected, actual }
    }

    /// Create a too few arguments error
    pub fn too_few_args(min: usize, actual: usize) -> Self {
        Self::TooFewArgs { min, actual }
    }

    /// Create a too many arguments error
    pub fn too_many_args(max: usize, actual: usize) -> Self {
        Self::TooManyArgs { max, actual }
    }

    /// Create a wrong argument count range error
    pub fn wrong_arg_range(min: usize, max: usize, actual: usize) -> Self {
        Self::WrongArgCount { min, max, actual }
    }

    /// Create a void variable error
    pub fn void_variable(symbol: Symbol) -> Self {
        Self::VoidVariable { symbol }
    }

    /// Create a void function error
    pub fn void_function(symbol: Symbol) -> Self {
        Self::VoidFunction { symbol }
    }

    /// Create an unbound symbol error
    pub fn unbound_symbol(symbol: Symbol) -> Self {
        Self::UnboundSymbol { symbol }
    }

    /// Create an invalid function error
    pub fn invalid_function(value: Object) -> Self {
        Self::InvalidFunction { value }
    }

    /// Create a user error
    pub fn user_error(message: impl Into<String>) -> Self {
        Self::UserError {
            message: message.into(),
        }
    }

    /// Create an internal error
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self::InternalError {
            message: message.into(),
        }
    }

    /// Wrap this error with additional context
    pub fn with_context(self, context: impl Into<String>) -> Self {
        Self::ContextualError {
            context: context.into(),
            source: Box::new(self),
        }
    }

    /// Check if this error should cause the interpreter to quit
    pub fn is_quit(&self) -> bool {
        matches!(self, Self::UserQuit)
    }

    /// Check if this error is recoverable
    pub fn is_recoverable(&self) -> bool {
        !matches!(
            self,
            Self::OutOfMemory
                | Self::StackOverflow
                | Self::RecursionLimit
                | Self::InternalError { .. }
                | Self::UserQuit
        )
    }

    /// Get the error category for debugging/logging
    pub fn category(&self) -> &'static str {
        match self {
            Self::WrongType { .. }
            | Self::WrongNumberOfArgs { .. }
            | Self::TooFewArgs { .. }
            | Self::TooManyArgs { .. }
            | Self::WrongArgCount { .. } => "type-error",

            Self::VoidVariable { .. }
            | Self::VoidFunction { .. }
            | Self::UnboundSymbol { .. }
            | Self::SettingConstant { .. } => "symbol-error",

            Self::InvalidFunction { .. }
            | Self::UndefinedFunction { .. }
            | Self::CircularFunction { .. } => "function-error",

            Self::DivisionByZero | Self::ArithmeticOverflow | Self::InvalidArithmetic { .. } => {
                "arithmetic-error"
            }

            Self::NotAList { .. }
            | Self::NotASequence { .. }
            | Self::IndexOutOfBounds { .. }
            | Self::CannotTakeCar { .. }
            | Self::CannotTakeCdr { .. } => "sequence-error",

            Self::StringIndexOutOfBounds { .. } | Self::InvalidStringOperation { .. } => {
                "string-error"
            }

            Self::VectorIndexOutOfBounds { .. } | Self::NotAVector { .. } => "vector-error",

            Self::NoCatch { .. }
            | Self::InvalidThrow
            | Self::ReturnOutsideFunction
            | Self::BreakOutsideLoop
            | Self::ContinueOutsideLoop => "control-flow-error",

            Self::UnhandledError { .. } | Self::InvalidCondition { .. } | Self::Signal { .. } => {
                "condition-error"
            }

            Self::FileNotFound { .. }
            | Self::PermissionDenied { .. }
            | Self::IoError { .. }
            | Self::EndOfFile => "io-error",

            Self::NoSuchBuffer { .. }
            | Self::ReadOnlyBuffer { .. }
            | Self::PointOutOfBounds { .. }
            | Self::InvalidMark => "buffer-error",

            Self::InvalidSyntax { .. }
            | Self::MalformedSpecialForm { .. }
            | Self::InvalidLambdaList { .. } => "syntax-error",

            Self::OutOfMemory
            | Self::StackOverflow
            | Self::RecursionLimit
            | Self::ResourceLimit { .. } => "resource-error",

            Self::MacroExpansionError { .. } | Self::InvalidMacroDefinition => "macro-error",

            Self::CommandNotFound { .. }
            | Self::InvalidInteractiveSpec { .. }
            | Self::UserQuit
            | Self::UserError { .. } => "user-error",

            Self::InternalError { .. }
            | Self::NotImplemented { .. }
            | Self::InvalidOperation { .. }
            | Self::ContextualError { .. } => "internal-error",
        }
    }
}

/// Result type for runtime operations
pub type RuntimeResult<T = Object> = Result<T, RuntimeError>;

/// Helper trait for converting values to runtime errors
pub trait IntoRuntimeError {
    fn into_runtime_error(self) -> RuntimeError;
}

impl IntoRuntimeError for std::io::Error {
    fn into_runtime_error(self) -> RuntimeError {
        match self.kind() {
            std::io::ErrorKind::NotFound => RuntimeError::FileNotFound {
                filename: "unknown".to_string(),
            },
            std::io::ErrorKind::PermissionDenied => RuntimeError::PermissionDenied {
                filename: "unknown".to_string(),
            },
            std::io::ErrorKind::UnexpectedEof => RuntimeError::EndOfFile,
            _ => RuntimeError::IoError {
                message: self.to_string(),
            },
        }
    }
}

impl From<std::io::Error> for RuntimeError {
    fn from(error: std::io::Error) -> Self {
        error.into_runtime_error()
    }
}

/// Macro for creating runtime errors with context
#[macro_export]
macro_rules! runtime_error {
    ($kind:ident) => {
        $crate::core::error::RuntimeError::$kind
    };
    ($kind:ident, $($field:ident: $value:expr),+ $(,)?) => {
        $crate::core::error::RuntimeError::$kind {
            $($field: $value),+
        }
    };
    ($kind:ident($($value:expr),+ $(,)?)) => {
        $crate::core::error::RuntimeError::$kind {
            $($value),+
        }
    };
}

/// Macro for early return with runtime error
#[macro_export]
macro_rules! runtime_bail {
    ($($args:tt)*) => {
        return Err(runtime_error!($($args)*))
    };
}

/// Macro for ensuring a condition or return runtime error
#[macro_export]
macro_rules! runtime_ensure {
    ($cond:expr, $($args:tt)*) => {
        if !($cond) {
            runtime_bail!($($args)*);
        }
    };
}
