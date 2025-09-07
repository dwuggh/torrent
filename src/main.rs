use chumsky::Parser;

use crate::{
    ast::elisp_parser,
    core::{
        compiler::{
            ast_to_ir::node_to_ir,
            jit::JIT,
            scope::{CompileScope, GlobalScope},
        },
        env::Environment,
        value::Value,
    },
};

pub mod ast;
pub mod core;
pub mod gc;

fn main() -> anyhow::Result<()> {
    // let subscriber = tracing_subscriber::fmt().with_max_level(tracing::Level::TRACE).finish();
    // tracing::subscriber::set_global_default(subscriber)?;
    tracing_subscriber::fmt::init();
    let mut jit = JIT::default();
    let text = "(let ((x (lambda (x) 1))) x)";
    // let text = "((lambda (x) x) 20)";
    // let text = "(let ((x 5)) x)";
    // let text = "2";
    let runtime_env = Box::new(Environment::default());
    let root = GlobalScope::new(&runtime_env);
    let ptr = runtime_env.as_ref() as *const Environment;
    // let runtime_env = Box::new(Environment::default());

    let ctx = CompileScope::Global(root);

    unsafe {
        let a: u64 = run_code(&mut jit, text, ctx, ptr).unwrap();
        let value = Value(a);
        let result = value.untag();
        println!("result: {result:?} {a}");
    };
    Ok(())
}

unsafe fn run_code<O>(
    jit: &mut JIT,
    code: &str,
    ctx: CompileScope<'_>,
    env: *const Environment,
) -> anyhow::Result<O> {
    unsafe {
        // Pass the string to the JIT, and it returns a raw pointer to machine code.
        let node = elisp_parser().parse(code).unwrap()[0].clone();
        let expr = node_to_ir(node)?;

        let f = jit.compile_expr(&expr, &ctx).unwrap();
        // Cast the raw pointer to a typed function pointer. This is unsafe, because
        // this is the critical point where you have to trust that the generated code
        // is safe to be called.
        let code_fn = std::mem::transmute::<_, fn(i64, u64, *const Environment) -> O>(f);
        // And now we can call it!
        Ok(code_fn(0, 0, env as *const Environment))
    }
}
