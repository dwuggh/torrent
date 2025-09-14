//! Optimization pipeline orchestration.

use super::{
    passes::*,
    ir::{ScopedExpr, TypedExpr, OptimizedExpr},
    OptimizationError, OptimizationResult,
};
use crate::core::compiler::ir::Expr;
use std::collections::{HashMap, HashSet};

/// Configuration for optimization pipeline
#[derive(Debug, Clone)]
pub struct OptimizationConfig {
    pub optimization_level: u8,  // 0-3
    pub inline_threshold: u32,
    pub unroll_threshold: u32,
    pub max_inline_depth: u32,
    pub enable_specialization: bool,
    pub enable_vectorization: bool,
    pub target_features: Vec<String>,
    pub debug_mode: bool,
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self {
            optimization_level: 1,
            inline_threshold: 50,
            unroll_threshold: 4,
            max_inline_depth: 3,
            enable_specialization: true,
            enable_vectorization: false,
            target_features: Vec::new(),
            debug_mode: false,
        }
    }
}

/// Orchestrates the optimization pipeline
pub struct OptimizationPipeline {
    config: OptimizationConfig,
    pass_registry: HashMap<String, Box<dyn PassRunner>>,
    pass_order: Vec<String>,
}

trait PassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput>;
    fn name(&self) -> &str;
    fn dependencies(&self) -> Vec<&str>;
}

#[derive(Debug)]
enum PassInput {
    Raw(Expr),
    Scoped(ScopedExpr),
    Typed(TypedExpr),
    Optimized(OptimizedExpr),
}

#[derive(Debug)]
enum PassOutput {
    Scoped(ScopedExpr),
    Typed(TypedExpr),
    Optimized(OptimizedExpr),
}

impl OptimizationPipeline {
    pub fn new(config: OptimizationConfig) -> OptimizationResult<Self> {
        let mut pipeline = Self {
            config,
            pass_registry: HashMap::new(),
            pass_order: Vec::new(),
        };
        
        pipeline.register_default_passes();
        pipeline.build_pass_order()?;
        Ok(pipeline)
    }
    
    pub fn optimize(&mut self, expr: Expr) -> OptimizationResult<OptimizedExpr> {
        let mut current_input = PassInput::Raw(expr);
        
        for pass_name in &self.pass_order.clone() {
            if let Some(pass) = self.pass_registry.get_mut(pass_name) {
                if self.should_run_pass(pass_name, &current_input) {
                    let output = pass.run_pass(current_input)?;
                    current_input = self.output_to_input(output);
                }
            }
        }
        
        match current_input {
            PassInput::Optimized(optimized) => Ok(optimized),
            _ => Err(OptimizationError::PassFailed {
                pass: "pipeline".to_string(),
                reason: "Pipeline did not produce optimized output".to_string(),
            }),
        }
    }
    
    fn register_default_passes(&mut self) {
        // Register all optimization passes
        self.register_pass(Box::new(ScopeResolutionPassRunner::new()));
        self.register_pass(Box::new(TypeInferencePassRunner::new()));
        self.register_pass(Box::new(DeadCodeEliminationPassRunner::new()));
        self.register_pass(Box::new(ConstantFoldingPassRunner::new()));
        self.register_pass(Box::new(InliningPassRunner::new(
            self.config.inline_threshold,
            self.config.max_inline_depth,
        )));
        
        if self.config.enable_specialization {
            self.register_pass(Box::new(SpecializationPassRunner::new()));
        }
        
        self.register_pass(Box::new(LoopOptimizationPassRunner::new()));
    }
    
    fn register_pass(&mut self, pass: Box<dyn PassRunner>) {
        let name = pass.name().to_string();
        self.pass_registry.insert(name.clone(), pass);
    }
    
    fn build_pass_order(&mut self) -> OptimizationResult<()> {
        // Topological sort of passes based on dependencies
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();
        let mut order = Vec::new();
        
        for pass_name in self.pass_registry.keys() {
            if !visited.contains(pass_name) {
                self.visit_pass(
                    pass_name,
                    &mut visited,
                    &mut temp_visited,
                    &mut order,
                )?;
            }
        }
        
        self.pass_order = order;
        Ok(())
    }
    
    fn visit_pass(
        &self,
        pass_name: &str,
        visited: &mut HashSet<String>,
        temp_visited: &mut HashSet<String>,
        order: &mut Vec<String>,
    ) -> OptimizationResult<()> {
        if temp_visited.contains(pass_name) {
            return Err(OptimizationError::ConfigError {
                message: format!("Circular dependency detected involving pass: {}", pass_name),
            });
        }
        
        if visited.contains(pass_name) {
            return Ok(());
        }
        
        temp_visited.insert(pass_name.to_string());
        
        if let Some(pass) = self.pass_registry.get(pass_name) {
            for dep in pass.dependencies() {
                self.visit_pass(dep, visited, temp_visited, order)?;
            }
        }
        
        temp_visited.remove(pass_name);
        visited.insert(pass_name.to_string());
        order.push(pass_name.to_string());
        
        Ok(())
    }
    
    fn should_run_pass(&self, pass_name: &str, input: &PassInput) -> bool {
        match self.config.optimization_level {
            0 => pass_name == "scope-resolution", // Only basic passes
            1 => !matches!(pass_name, "specialization" | "loop-optimization"),
            2 => pass_name != "loop-optimization",
            3 => true, // All passes
            _ => true,
        }
    }
    
    fn output_to_input(&self, output: PassOutput) -> PassInput {
        match output {
            PassOutput::Scoped(scoped) => PassInput::Scoped(scoped),
            PassOutput::Typed(typed) => PassInput::Typed(typed),
            PassOutput::Optimized(optimized) => PassInput::Optimized(optimized),
        }
    }
}

// Pass runner implementations
struct ScopeResolutionPassRunner {
    pass: ScopeResolutionPass,
}

impl ScopeResolutionPassRunner {
    fn new() -> Self {
        Self {
            pass: ScopeResolutionPass::new(),
        }
    }
}

impl PassRunner for ScopeResolutionPassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Raw(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Scoped(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected raw expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "scope-resolution" }
    fn dependencies(&self) -> Vec<&str> { vec![] }
}

struct TypeInferencePassRunner {
    pass: TypeInferencePass,
}

impl TypeInferencePassRunner {
    fn new() -> Self {
        Self {
            pass: TypeInferencePass::new(),
        }
    }
}

impl PassRunner for TypeInferencePassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Scoped(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Typed(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected scoped expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "type-inference" }
    fn dependencies(&self) -> Vec<&str> { vec!["scope-resolution"] }
}

struct DeadCodeEliminationPassRunner {
    pass: DeadCodeEliminationPass,
}

impl DeadCodeEliminationPassRunner {
    fn new() -> Self {
        Self {
            pass: DeadCodeEliminationPass::default(),
        }
    }
}

impl PassRunner for DeadCodeEliminationPassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Typed(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Typed(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected typed expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "dead-code-elimination" }
    fn dependencies(&self) -> Vec<&str> { vec!["scope-resolution", "type-inference"] }
}

struct ConstantFoldingPassRunner {
    pass: ConstantFoldingPass,
}

impl ConstantFoldingPassRunner {
    fn new() -> Self {
        Self {
            pass: ConstantFoldingPass::default(),
        }
    }
}

impl PassRunner for ConstantFoldingPassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Typed(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Typed(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected typed expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "constant-folding" }
    fn dependencies(&self) -> Vec<&str> { vec![] }
}

struct InliningPassRunner {
    pass: InliningPass,
}

impl InliningPassRunner {
    fn new(threshold: u32, max_depth: u32) -> Self {
        Self {
            pass: InliningPass::new(threshold, max_depth),
        }
    }
}

impl PassRunner for InliningPassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Typed(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Typed(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected typed expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "inlining" }
    fn dependencies(&self) -> Vec<&str> { vec!["type-inference", "constant-folding"] }
}

struct SpecializationPassRunner {
    pass: SpecializationPass,
}

impl SpecializationPassRunner {
    fn new() -> Self {
        Self {
            pass: SpecializationPass::default(),
        }
    }
}

impl PassRunner for SpecializationPassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Typed(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Optimized(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected typed expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "specialization" }
    fn dependencies(&self) -> Vec<&str> { vec!["type-inference", "inlining"] }
}

struct LoopOptimizationPassRunner {
    pass: LoopOptimizationPass,
}

impl LoopOptimizationPassRunner {
    fn new() -> Self {
        Self {
            pass: LoopOptimizationPass::default(),
        }
    }
}

impl PassRunner for LoopOptimizationPassRunner {
    fn run_pass(&mut self, input: PassInput) -> OptimizationResult<PassOutput> {
        match input {
            PassInput::Typed(expr) => {
                let result = self.pass.run(expr)?;
                Ok(PassOutput::Typed(result))
            }
            _ => Err(OptimizationError::PassFailed {
                pass: self.name().to_string(),
                reason: "Expected typed expression input".to_string(),
            }),
        }
    }
    
    fn name(&self) -> &str { "loop-optimization" }
    fn dependencies(&self) -> Vec<&str> { vec![] }
}
