use chumsky::Parser;

use crate::{
    ast::elisp_parser,
    core::{
        compiler::{ast_to_ir::node_to_ir, jit::JIT, scope::CompileScope},
        env::Environment,
        value::{LispValue, Value},
    },
};

pub mod ast;
pub mod core;
pub mod gc;

fn main() -> anyhow::Result<()> {
    // let subscriber = tracing_subscriber::fmt().with_max_level(tracing::Level::TRACE).finish();
    // tracing::subscriber::set_global_default(subscriber)?;
    tracing_subscriber::fmt::init();
    // let text = "(let ((x (lambda (x) x))) (x 4))";
    let text = include_str!("test.el");
    // let text = "((lambda (x) x) 20)";
    // let text = "(let ((x 5)) x)";
    // let text = "2";
    let runtime_env = Environment::default();
    let mut jit = JIT::new(&runtime_env);
    // runtime_env.init_nil_t();
    let ptr = &runtime_env as *const Environment;
    // let runtime_env = Box::new(Environment::default());

    let ctx = CompileScope::global();

    unsafe {
        let result = run_code(&mut jit, text, ctx, ptr).unwrap();
        // let value = Value(a);
        // let result = value.untag();
        println!("result: {result:?}");
    };
    Ok(())
}

unsafe fn run_code(
    jit: &mut JIT,
    code: &str,
    ctx: CompileScope<'_>,
    env: *const Environment,
) -> anyhow::Result<Vec<anyhow::Result<LispValue>>> {
    unsafe {
        // Pass the string to the JIT, and it returns a raw pointer to machine code.
        let node = elisp_parser().parse(code).unwrap();
        let vals = node.into_iter().map(node_to_ir).map(|expr| {
            let expr = expr?;
            let f = jit.compile_expr(&expr, &ctx).unwrap();
            let code_fn = std::mem::transmute::<_, fn(*const Environment) -> Value>(f);
            // And now we can call it!
            let value = code_fn(env as *const Environment);
            let result = value.untag();
            println!("result: {result:?}");
            Ok(result)
        }).collect::<Vec<_>>();
        Ok(vals)
    }
}
