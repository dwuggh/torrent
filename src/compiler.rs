use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FloatValue, FunctionValue, PointerValue};

use std::collections::HashMap;
use anyhow::anyhow;

use crate::ast::Node;

/// Manages the state of the compilation process.
pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    // For managing variables
    variables: HashMap<String, PointerValue<'ctx>>,
    // For managing functions
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    // Inside impl<'ctx> Compiler<'ctx> {

    /// Compiles a single AST Node into an LLVM value.
    fn compile_node(&mut self, node: &Node) -> anyhow::Result<FloatValue<'ctx>> {
        match node {
            Node::Integer(n) => {
                // Treat all numbers as floats for simplicity (a common Lisp practice)
                Ok(self.context.f64_type().const_float(*n as f64))
            }
            Node::Float(f) => Ok(self.context.f64_type().const_float(*f)),
            Node::Symbol(name) => {
                // Look up the variable in our symbol table
                match self.variables.get(name.as_str()) {
                    Some(ptr) => {
                        // Load the value from the memory location (pointer)
                        Ok(self
                            .builder
                            .build_load(self.context.f64_type(), *ptr, name)
                            .unwrap()
                            .into_float_value())
                    }
                    None => Err(anyhow!("Unknown variable: {}", name)),
                }
            }
            Node::Sexp(list) => {
                // This is the most complex part, handled in the next step
                self.compile_sexp(list)
            }
            // Other nodes like Str, Nil, etc., will require a more complex runtime
            _ => Err(anyhow!("Unsupported node type for compilation")),
        }
    }

    // Inside impl<'ctx> Compiler<'ctx> {

    fn compile_sexp(&mut self, list: &[Node]) -> anyhow::Result<FloatValue<'ctx>> {
        if list.is_empty() {
            return Err(anyhow!("Cannot compile empty list"));
        }

        let head = &list[0];
        let args = &list[1..];

        if let Node::Symbol(op) = head {
            match op.as_str() {
                // --- Special Forms ---
                "if" => self.compile_if(args),
                // "let", "defun", "quote" will be other special forms

                // --- Regular Function Call ---
                _ => self.compile_fn_call(op, args),
            }
        } else {
            Err(anyhow!("Expected a symbol at the head of the list"))
        }
    }

    // Inside impl<'ctx> Compiler<'ctx> {

    fn compile_if(&mut self, args: &[Node]) -> anyhow::Result<FloatValue<'ctx>> {
        if args.len() != 3 {
            anyhow::bail!("if requires 3 arguments");
        }

        let cond_node = &args[0];
        let then_node = &args[1];
        let else_node = &args[2];

        let cond_val = self.compile_node(cond_node)?;

        // Compare condition with 0.0
        let zero = self.context.f64_type().const_float(0.0);
        let comparison = self.builder.build_float_compare(
            inkwell::FloatPredicate::ONE, // ONE = Ordered Not Equal
            cond_val,
            zero,
            "ifcond",
        )?;

        // Get the current function to create blocks within it
        let function = self.fn_value_opt.unwrap();

        // Create blocks for the `then`, `else`, and `merge` cases
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "merge");

        // Create the conditional branch
        self.builder
            .build_conditional_branch(comparison, then_bb, else_bb)?;

        // --- Build the `then` block ---
        self.builder.position_at_end(then_bb);
        let then_val = self.compile_node(then_node)?;
        self.builder.build_unconditional_branch(merge_bb)?; // Jump to merge after
        let then_bb = self.builder.get_insert_block().unwrap(); // Update block reference

        // --- Build the `else` block ---
        self.builder.position_at_end(else_bb);
        let else_val = self.compile_node(else_node)?;
        self.builder.build_unconditional_branch(merge_bb)?;
        let else_bb = self.builder.get_insert_block().unwrap();

        // --- Build the `merge` block ---
        self.builder.position_at_end(merge_bb);
        // Use a PHI node to get the value from whichever branch was taken
        let phi = self.builder.build_phi(self.context.f64_type(), "iftmp")?;
        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        Ok(phi.as_basic_value().into_float_value())
    }

    // Inside impl<'ctx> Compiler<'ctx> {

    fn compile_fn_call(&mut self, op: &str, args: &[Node]) -> anyhow::Result<FloatValue<'ctx>> {
        let function = match self.module.get_function(op) {
            Some(f) => f,
            None => return Err(anyhow::anyhow!("Unknown function: {}", op)),
        };

        let mut compiled_args = Vec::with_capacity(args.len());
        for arg in args {
            compiled_args.push(self.compile_node(arg)?.into());
        }

        match self
            .builder
            .build_call(function, &compiled_args, "calltmp")?
            .try_as_basic_value()
            .left()
        {
            Some(val) => Ok(val.into_float_value()),
            None => anyhow::bail!("Function call did not return a value".to_string()),
        }
    }
}
