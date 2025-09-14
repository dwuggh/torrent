//! Intermediate representation for optimization passes.

use crate::{
    ast::Node,
    core::{
        compiler::ir::{Expr, Literal},
        ident::Ident,
        symbol::Symbol,
        number::{LispCharacter, LispFloat, LispInteger},
        string::LispStr,
    },
};
use std::collections::HashMap;

/// Source code span for debugging and error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
}

impl Span {
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self { start, end, line, column }
    }
    
    pub fn dummy() -> Self {
        Self { start: 0, end: 0, line: 0, column: 0 }
    }
    
    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

/// Raw expression after initial parsing
#[derive(Debug, Clone)]
pub struct RawExpr {
    pub kind: ExprKind,
    pub span: Span,
}

/// Expression with resolved scopes and variables
#[derive(Debug, Clone)]
pub struct ScopedExpr {
    pub kind: ExprKind,
    pub span: Span,
    pub scope_info: ScopeInfo,
}

/// Expression with type hints and inference
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: ExprKind,
    pub span: Span,
    pub scope_info: ScopeInfo,
    pub type_hint: TypeHint,
}

/// Fully optimized expression
#[derive(Debug, Clone)]
pub struct OptimizedExpr {
    pub kind: OptimizedExprKind,
    pub span: Span,
    pub scope_info: ScopeInfo,
    pub type_hint: TypeHint,
    pub optimization_info: OptimizationInfo,
}

/// Expression kinds for optimization IR
#[derive(Debug, Clone)]
pub enum ExprKind {
    Nil,
    Literal(Literal),
    Symbol(Ident),      // Will be replaced with Variable(Var) after scope resolution
    Variable(Var),      // Resolved variable reference
    Vector(Vec<ExprKind>),
    Call(Call),
    SpecialForm(SpecialForm),
}

/// Variable reference types
#[derive(Clone, Debug)]
pub enum Var {
    Symbol(Symbol),
    Local(Local),
    Capture(Capture),
}

/// Local variable
#[derive(Clone, Debug)]
pub struct Local {
    pub ident: Ident,
    pub id: u32,
    pub depth: u32,
}

/// Captured variable from outer scope
#[derive(Clone, Debug)]
pub struct Capture {
    pub ident: Ident,
    pub id: u32,
    pub source_depth: u32,  // Depth where variable was originally defined
}

/// Function call
#[derive(Debug, Clone)]
pub struct Call {
    pub func: Box<ExprKind>,
    pub args: Vec<ExprKind>,
}

/// Special forms
#[derive(Debug, Clone)]
pub enum SpecialForm {
    Let(Let),
    Lambda(Lambda),
    If(If),
    // Add other special forms as needed
}

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<(Ident, Option<ExprKind>)>,
    pub body: Vec<ExprKind>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub args: Vec<Arg>,
    pub docstring: Option<String>,
    pub body: Vec<ExprKind>,
    pub captures: Vec<Ident>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<ExprKind>,
    pub then_branch: Box<ExprKind>,
    pub else_branch: Option<Box<ExprKind>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Arg {
    pub ty: ArgsType,
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ArgsType {
    Normal,
    Optional,
    Rest,
}

/// Scope and variable information
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    pub variable_usage: HashMap<Ident, VariableUsage>,
    pub escape_analysis: HashMap<Ident, EscapeInfo>,
    pub live_ranges: HashMap<Ident, VariableLifetime>,
    pub scope_depth: u32,
}

#[derive(Debug, Clone)]
pub struct VariableUsage {
    pub reads: u32,
    pub writes: u32,
    pub is_captured: bool,
    pub capture_depth: Option<u32>,
    pub first_use_span: Span,
    pub last_use_span: Span,
}

#[derive(Debug, Clone)]
pub enum EscapeInfo {
    NoEscape,           // Variable doesn't escape current scope
    EscapesToParent,    // Captured by inner function
    EscapesToHeap,      // Stored in data structure
    Unknown,            // Analysis incomplete
}

#[derive(Debug, Clone)]
pub struct VariableLifetime {
    pub first_use: usize,    // Instruction index
    pub last_use: usize,     // Instruction index
    pub live_ranges: Vec<(usize, usize)>,
    pub interference: Vec<Ident>, // Variables that interfere with this one
}

/// Type hint information
#[derive(Debug, Clone)]
pub enum TypeHint {
    Unknown,
    Inferred(InferredType),
    Declared(DeclaredType),
    Specialized(SpecializedType),
}

#[derive(Debug, Clone)]
pub enum InferredType {
    Nil,
    Integer,
    Float,
    Character,
    String,
    Symbol,
    List(Box<InferredType>),
    Vector(Box<InferredType>),
    Function(FunctionType),
    Union(Vec<InferredType>),
    Any, // Top type
}

#[derive(Debug, Clone)]
pub struct DeclaredType {
    pub base_type: InferredType,
    pub source: TypeSource,
}

#[derive(Debug, Clone)]
pub enum TypeSource {
    UserAnnotation,
    CompilerDirective,
    InferredFromUsage,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<InferredType>,
    pub return_type: Box<InferredType>,
    pub is_pure: bool,        // No side effects
    pub is_total: bool,       // Always terminates
    pub arity: FunctionArity,
}

#[derive(Debug, Clone)]
pub enum FunctionArity {
    Fixed(usize),
    Variable(usize), // Minimum number of args
    Any,
}

#[derive(Debug, Clone)]
pub struct SpecializedType {
    pub base_type: InferredType,
    pub constraints: Vec<TypeConstraint>,
    pub specialization_key: String,
}

#[derive(Debug, Clone)]
pub enum TypeConstraint {
    Range(i64, i64),          // Integer range
    Length(usize),            // String/list length
    NonNull,                  // Not nil
    Immutable,                // Read-only
    Positive,                 // > 0
    NonEmpty,                 // Non-empty collection
}

/// Optimization metadata
#[derive(Debug, Clone)]
pub struct OptimizationInfo {
    pub constant_folding: ConstantInfo,
    pub inlining: InliningInfo,
    pub specialization: SpecializationInfo,
    pub loop_info: Option<LoopInfo>,
}

#[derive(Debug, Clone)]
pub enum ConstantInfo {
    NotConstant,
    CompileTimeConstant(Literal),
    PartiallyConstant(Vec<usize>), // Which arguments are constant
    PureFunction, // Function with no side effects
}

#[derive(Debug, Clone)]
pub struct InliningInfo {
    pub should_inline: bool,
    pub inline_cost: u32,
    pub call_frequency: u32,
    pub inline_depth: u32,
    pub reasons: Vec<InlineReason>,
}

#[derive(Debug, Clone)]
pub enum InlineReason {
    SmallFunction,
    HighFrequency,
    ConstantArguments,
    UserDirective,
    TailCall,
}

#[derive(Debug, Clone)]
pub struct SpecializationInfo {
    pub specialized_versions: Vec<SpecializedVersion>,
    pub dispatch_strategy: DispatchStrategy,
    pub monomorphization_key: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SpecializedVersion {
    pub type_signature: Vec<InferredType>,
    pub optimized_body: Box<OptimizedExpr>,
    pub performance_gain: f64,
}

#[derive(Debug, Clone)]
pub enum DispatchStrategy {
    Dynamic,              // Runtime type checking
    Static,               // Compile-time resolved
    Polymorphic,          // Multiple specialized versions
    Monomorphic,          // Single specialized version
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
    pub loop_type: LoopType,
    pub iteration_count: Option<u64>,
    pub optimizations: Vec<LoopOptimization>,
}

#[derive(Debug, Clone)]
pub enum LoopType {
    While,
    For,
    Recursive,
    TailRecursive,
}

#[derive(Debug, Clone)]
pub enum LoopOptimization {
    Unrolled(u32),        // Loop unrolling factor
    Vectorized,           // SIMD optimization
    StrengthReduced,      // Replace expensive ops with cheaper ones
    Hoisted(Vec<Ident>),  // Hoisted invariant expressions
}

/// Optimized expression kinds
#[derive(Debug, Clone)]
pub enum OptimizedExprKind {
    // Original expressions
    Variable(Var),
    Literal(Literal),
    Call(OptimizedCall),
    SpecialForm(OptimizedSpecialForm),
    
    // Optimized forms
    InlinedCall {
        original_call: OptimizedCall,
        inlined_body: Box<OptimizedExpr>,
        inline_depth: u32,
    },
    SpecializedCall {
        original_call: OptimizedCall,
        specialized_version: usize,
        type_signature: Vec<InferredType>,
    },
    ConstantFolded {
        original_expr: Box<OptimizedExpr>,
        folded_value: Literal,
    },
    DeadCodeEliminated {
        reason: String,
        original_span: Span,
    },
    LoopOptimized {
        original_loop: Box<OptimizedExpr>,
        optimization: LoopOptimization,
    },
    Unreachable {
        reason: String,
    },
}

#[derive(Debug, Clone)]
pub struct OptimizedCall {
    pub func: Box<OptimizedExpr>,
    pub args: Vec<OptimizedExpr>,
    pub call_type: CallType,
}

#[derive(Debug, Clone)]
pub enum CallType {
    Direct,      // Direct function call
    Indirect,    // Function pointer call
    Builtin,     // Built-in function
    Macro,       // Macro expansion
    TailCall,    // Tail call optimization
}

#[derive(Debug, Clone)]
pub enum OptimizedSpecialForm {
    // Add optimized versions of special forms
    If {
        condition: Box<OptimizedExpr>,
        then_branch: Box<OptimizedExpr>,
        else_branch: Option<Box<OptimizedExpr>>,
        is_constant_condition: bool,
    },
    Let {
        bindings: Vec<(Local, Option<OptimizedExpr>)>,
        body: Vec<OptimizedExpr>,
        eliminated_bindings: Vec<Ident>, // Dead bindings
    },
    Lambda {
        params: Vec<Local>,
        body: Vec<OptimizedExpr>,
        captures: Vec<Capture>,
        is_pure: bool,
        inline_hint: bool,
    },
}

// Default implementations
impl Default for ScopeInfo {
    fn default() -> Self {
        Self {
            variable_usage: HashMap::new(),
            escape_analysis: HashMap::new(),
            live_ranges: HashMap::new(),
            scope_depth: 0,
        }
    }
}

impl Default for TypeHint {
    fn default() -> Self {
        TypeHint::Unknown
    }
}

impl Default for OptimizationInfo {
    fn default() -> Self {
        Self {
            constant_folding: ConstantInfo::NotConstant,
            inlining: InliningInfo {
                should_inline: false,
                inline_cost: 0,
                call_frequency: 0,
                inline_depth: 0,
                reasons: Vec::new(),
            },
            specialization: SpecializationInfo {
                specialized_versions: Vec::new(),
                dispatch_strategy: DispatchStrategy::Dynamic,
                monomorphization_key: None,
            },
            loop_info: None,
        }
    }
}

// Conversion implementations
impl From<RawExpr> for ScopedExpr {
    fn from(raw: RawExpr) -> Self {
        Self {
            kind: raw.kind,
            span: raw.span,
            scope_info: ScopeInfo::default(),
        }
    }
}

impl From<ScopedExpr> for TypedExpr {
    fn from(scoped: ScopedExpr) -> Self {
        Self {
            kind: scoped.kind,
            span: scoped.span,
            scope_info: scoped.scope_info,
            type_hint: TypeHint::Unknown,
        }
    }
}
