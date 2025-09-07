// use ast::Node;
// use env::{Env, Value};

// mod ast;
// mod env;

// fn main() {
//     let program = "(+ 6 1)";
//     let nodes = Node::parse(program).unwrap();
//     let mut env = Env::new();
//     for node in nodes {
//         let val = env.eval_node(node);
//         match val {
//             Value::Integer(i) => println!("Result: {}", i),
//             Value::Float(f) => println!("Result: {}", f),
//             _ => println!("Result: {:?}", val),
//         }
//     }
// }

use chumsky::Parser;

use crate::{
    ast::elisp_parser,
    core::{
        compiler::{
            ast_to_ir::node_to_ir, ir::Expr, jit::JIT, scope::{CompileScope, GlobalScope}
        },
        env::Environment,
        value::Value,
    },
};

pub mod ast;
pub mod core;
pub mod gc;

fn main() -> anyhow::Result<()> {
    let mut jit = JIT::default();
    let text = "(+ 1 2)";
    // let text = "2";
    let runtime_env = Environment::default();
    let root = GlobalScope::new(&runtime_env);
    let ctx = CompileScope::Global(root);

    unsafe {
        let a: u64 = run_code(&mut jit, text, ctx, ()).unwrap();
        let value = Value(a);
        let result = value.untag();
        println!("result: {result:?}");
    };
    Ok(())
}

unsafe fn run_code<I, O>(
    jit: &mut JIT,
    code: &str,
    ctx: CompileScope<'_>,
    input: I,
) -> anyhow::Result<O> {
    unsafe {
        // Pass the string to the JIT, and it returns a raw pointer to machine code.
        let node = elisp_parser().parse(code).unwrap()[0].clone();
        println!("{node:?}");
        let expr = node_to_ir(node)?;

        let f = jit.compile_expr(&expr, &ctx).unwrap();
        // Cast the raw pointer to a typed function pointer. This is unsafe, because
        // this is the critical point where you have to trust that the generated code
        // is safe to be called.
        let code_fn = std::mem::transmute::<_, fn(I) -> O>(f);
        // And now we can call it!
        Ok(code_fn(input))
    }
}
