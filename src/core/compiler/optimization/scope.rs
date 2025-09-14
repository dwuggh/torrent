//! Scope analysis and variable resolution.

use super::{
    ir::{ScopedExpr, ScopeInfo, VariableUsage, EscapeInfo, VariableLifetime, Span, ExprKind, Var, Local, Capture, SpecialForm, Call, Let, Lambda, If, Arg, ArgsType},
    OptimizationError, OptimizationResult,
};
use crate::core::{
    compiler::ir::Expr,
    ident::Ident,
    symbol::Symbol,
};
use std::collections::HashMap;

/// Tracks variable scopes and resolves variable references
#[derive(Debug)]
pub struct ScopeAnalyzer {
    scopes: Vec<Scope>,
    next_local_id: u32,
    captures: Vec<Capture>,
    current_instruction: usize,
}

#[derive(Debug, Clone)]
pub struct Scope {
    bindings: HashMap<Ident, Local>,
    depth: u32,
    scope_type: ScopeType,
    parent_captures: Vec<Capture>,
}

#[derive(Debug, Clone)]
pub enum ScopeType {
    Function,    // Function parameters
    Let,         // let bindings
    LetStar,     // let* bindings
    Lambda,      // lambda parameters
    Block,       // General block scope
}

impl ScopeAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            next_local_id: 0,
            captures: Vec::new(),
            current_instruction: 0,
        }
    }

    pub fn analyze(&mut self, expr: Expr) -> OptimizationResult<ScopedExpr> {
        let scoped_kind = self.analyze_expr_kind(self.expr_to_kind(expr)?)?;
        
        Ok(ScopedExpr {
            kind: scoped_kind,
            span: Span::dummy(), // TODO: Extract from original expr
            scope_info: self.build_scope_info(),
        })
    }

    fn expr_to_kind(&self, expr: Expr) -> OptimizationResult<ExprKind> {
        match expr {
            Expr::Nil => Ok(ExprKind::Nil),
            Expr::Literal(lit) => Ok(ExprKind::Literal(lit)),
            Expr::Symbol(ident) => Ok(ExprKind::Symbol(ident)),
            Expr::Vector(exprs) => {
                let mut kinds = Vec::new();
                for e in exprs {
                    kinds.push(self.expr_to_kind(e)?);
                }
                Ok(ExprKind::Vector(kinds))
            }
            Expr::Call(call) => {
                let func_kind = Box::new(self.expr_to_kind(*call.func)?);
                let mut arg_kinds = Vec::new();
                for arg in call.args {
                    arg_kinds.push(self.expr_to_kind(arg)?);
                }
                Ok(ExprKind::Call(Call {
                    func: func_kind,
                    args: arg_kinds,
                }))
            }
            Expr::SpecialForm(special) => {
                let special_kind = self.special_form_to_kind(special)?;
                Ok(ExprKind::SpecialForm(special_kind))
            }
        }
    }

    fn special_form_to_kind(&self, special: crate::core::compiler::ir::SpecialForm) -> OptimizationResult<SpecialForm> {
        use crate::core::compiler::ir::SpecialForm as IrSpecialForm;
        
        match special {
            IrSpecialForm::Let(let_expr) => {
                let mut bindings = Vec::new();
                for (ident, value_opt) in let_expr.bindings {
                    let value_kind = if let Some(value) = value_opt {
                        Some(self.expr_to_kind(value)?)
                    } else {
                        None
                    };
                    bindings.push((ident, value_kind));
                }
                
                let mut body_kinds = Vec::new();
                for body_expr in let_expr.body {
                    body_kinds.push(self.expr_to_kind(body_expr)?);
                }
                
                Ok(SpecialForm::Let(Let {
                    bindings,
                    body: body_kinds,
                }))
            }
            IrSpecialForm::Lambda(lambda) => {
                let mut args = Vec::new();
                for arg in lambda.args {
                    args.push(Arg {
                        ty: match arg.ty {
                            crate::core::compiler::ir::ArgsType::Normal => ArgsType::Normal,
                            crate::core::compiler::ir::ArgsType::Optional => ArgsType::Optional,
                            crate::core::compiler::ir::ArgsType::Rest => ArgsType::Rest,
                        },
                        ident: arg.ident,
                    });
                }
                
                let mut body_kinds = Vec::new();
                for body_expr in lambda.body {
                    body_kinds.push(self.expr_to_kind(body_expr)?);
                }
                
                Ok(SpecialForm::Lambda(Lambda {
                    args,
                    docstring: lambda.docstring,
                    body: body_kinds,
                    captures: lambda.captures,
                }))
            }
            IrSpecialForm::If(if_expr) => {
                let condition = Box::new(self.expr_to_kind(*if_expr.cond)?);
                let then_branch = Box::new(self.expr_to_kind(*if_expr.then)?);
                let else_branch = if !if_expr.els.is_empty() {
                    // Convert multiple else expressions to a single expression
                    if if_expr.els.len() == 1 {
                        Some(Box::new(self.expr_to_kind(if_expr.els.into_iter().next().unwrap())?))
                    } else {
                        // Wrap multiple expressions in a progn-like structure
                        let mut else_kinds = Vec::new();
                        for else_expr in if_expr.els {
                            else_kinds.push(self.expr_to_kind(else_expr)?);
                        }
                        // For now, just take the last expression
                        Some(Box::new(else_kinds.into_iter().last().unwrap()))
                    }
                } else {
                    None
                };
                
                Ok(SpecialForm::If(If {
                    condition,
                    then_branch,
                    else_branch,
                }))
            }
            _ => Err(OptimizationError::ScopeError {
                message: "Unsupported special form for scope analysis".to_string(),
            }),
        }
    }

    fn analyze_expr_kind(&mut self, kind: ExprKind) -> OptimizationResult<ExprKind> {
        match kind {
            ExprKind::Symbol(ident) => {
                let var = self.resolve_variable(ident)?;
                self.record_variable_use(ident, false);
                Ok(ExprKind::Variable(var))
            }
            ExprKind::Call(call) => {
                let func = Box::new(self.analyze_expr_kind(*call.func)?);
                let mut args = Vec::new();
                for arg in call.args {
                    args.push(self.analyze_expr_kind(arg)?);
                }
                Ok(ExprKind::Call(Call { func, args }))
            }
            ExprKind::SpecialForm(special) => {
                let analyzed_special = self.analyze_special_form(special)?;
                Ok(ExprKind::SpecialForm(analyzed_special))
            }
            ExprKind::Vector(exprs) => {
                let mut analyzed_exprs = Vec::new();
                for expr in exprs {
                    analyzed_exprs.push(self.analyze_expr_kind(expr)?);
                }
                Ok(ExprKind::Vector(analyzed_exprs))
            }
            other => Ok(other), // Literals, etc.
        }
    }

    fn analyze_special_form(&mut self, special: SpecialForm) -> OptimizationResult<SpecialForm> {
        match special {
            SpecialForm::Let(let_expr) => {
                self.push_scope(ScopeType::Let);
                
                let mut analyzed_bindings = Vec::new();
                
                // Analyze binding values first (before variables are in scope)
                for (ident, value_opt) in let_expr.bindings {
                    let analyzed_value = if let Some(value) = value_opt {
                        Some(self.analyze_expr_kind(value)?)
                    } else {
                        None
                    };
                    
                    // Now bind the variable
                    let local = self.bind_local(ident)?;
                    analyzed_bindings.push((local.ident, analyzed_value));
                }
                
                // Analyze body
                let mut analyzed_body = Vec::new();
                for body_expr in let_expr.body {
                    analyzed_body.push(self.analyze_expr_kind(body_expr)?);
                }
                
                self.pop_scope();
                
                Ok(SpecialForm::Let(Let {
                    bindings: analyzed_bindings,
                    body: analyzed_body,
                }))
            }
            SpecialForm::Lambda(lambda) => {
                self.push_scope(ScopeType::Lambda);
                
                // Bind parameters
                let mut analyzed_args = Vec::new();
                for arg in lambda.args {
                    let local = self.bind_local(arg.ident)?;
                    analyzed_args.push(Arg {
                        ty: arg.ty,
                        ident: local.ident,
                    });
                }
                
                // Analyze body
                let mut analyzed_body = Vec::new();
                for body_expr in lambda.body {
                    analyzed_body.push(self.analyze_expr_kind(body_expr)?);
                }
                
                // Collect captures before popping scope
                let captures = self.captures.clone();
                self.pop_scope();
                
                Ok(SpecialForm::Lambda(Lambda {
                    args: analyzed_args,
                    docstring: lambda.docstring,
                    body: analyzed_body,
                    captures: captures.into_iter().map(|c| c.ident).collect(),
                }))
            }
            SpecialForm::If(if_expr) => {
                let condition = Box::new(self.analyze_expr_kind(*if_expr.condition)?);
                let then_branch = Box::new(self.analyze_expr_kind(*if_expr.then_branch)?);
                let else_branch = if let Some(else_expr) = if_expr.else_branch {
                    Some(Box::new(self.analyze_expr_kind(*else_expr)?))
                } else {
                    None
                };
                
                Ok(SpecialForm::If(If {
                    condition,
                    then_branch,
                    else_branch,
                }))
            }
        }
    }

    pub fn push_scope(&mut self, scope_type: ScopeType) {
        let depth = self.scopes.len() as u32;
        self.scopes.push(Scope {
            bindings: HashMap::new(),
            depth,
            scope_type,
            parent_captures: self.captures.clone(),
        });
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    pub fn bind_local(&mut self, ident: Ident) -> OptimizationResult<Local> {
        let local = Local {
            ident,
            id: self.next_local_id,
            depth: self.current_depth(),
        };
        self.next_local_id += 1;
        
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.bindings.insert(ident, local.clone());
        } else {
            return Err(OptimizationError::ScopeError {
                message: "No active scope for binding".to_string(),
            });
        }
        
        Ok(local)
    }

    pub fn resolve_variable(&mut self, ident: Ident) -> OptimizationResult<Var> {
        // Search from innermost to outermost scope
        for (scope_idx, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(local) = scope.bindings.get(&ident) {
                let current_depth = self.current_depth();
                
                if local.depth == current_depth {
                    // Same scope - it's a local
                    return Ok(Var::Local(local.clone()));
                } else {
                    // Different scope - it's a capture
                    let capture = Capture {
                        ident,
                        id: local.id,
                        source_depth: local.depth,
                    };
                    self.captures.push(capture.clone());
                    return Ok(Var::Capture(capture));
                }
            }
        }
        
        // Not found in any local scope - it's a global symbol
        Ok(Var::Symbol(Symbol::from(ident)))
    }

    fn record_variable_use(&mut self, ident: Ident, is_write: bool) {
        // Record usage for analysis
        self.current_instruction += 1;
    }

    fn current_depth(&self) -> u32 {
        self.scopes.len() as u32
    }

    fn build_scope_info(&self) -> ScopeInfo {
        // Build comprehensive scope information
        ScopeInfo {
            variable_usage: HashMap::new(), // TODO: Populate from analysis
            escape_analysis: HashMap::new(), // TODO: Populate from analysis
            live_ranges: HashMap::new(),     // TODO: Populate from analysis
            scope_depth: self.current_depth(),
        }
    }
}

/// Resolves variable references and performs escape analysis
pub struct VariableResolver {
    analyzer: ScopeAnalyzer,
}

impl VariableResolver {
    pub fn new() -> Self {
        Self {
            analyzer: ScopeAnalyzer::new(),
        }
    }

    pub fn resolve(&mut self, expr: Expr) -> OptimizationResult<ScopedExpr> {
        self.analyzer.analyze(expr)
    }

    pub fn analyze_escapes(&mut self, expr: &ScopedExpr) -> OptimizationResult<HashMap<Ident, EscapeInfo>> {
        // Perform escape analysis
        let mut escape_info = HashMap::new();
        
        // TODO: Implement escape analysis algorithm
        // - Track which variables are captured by closures
        // - Track which variables are stored in heap-allocated structures
        // - Track which variables escape their defining scope
        
        Ok(escape_info)
    }
}

impl Default for ScopeAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for VariableResolver {
    fn default() -> Self {
        Self::new()
    }
}
