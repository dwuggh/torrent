use chumsky::Parser;

use crate::{
    ast::elisp_parser,
    core::{
        compiler::{ast_to_ir::node_to_ir, jit::JIT, scope::CompileScope}, env::Environment, object::Object, string::LispStr, Tagged
    }, gc::collector::init_gc,
};

pub mod ast;
pub mod core;
pub mod gc;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    // init_gc();
    let text = include_str!("test.el");
    let runtime_env = Environment::default();
    let mut jit = JIT::new(&runtime_env);
    let ptr = &runtime_env as *const Environment;
    let ctx = CompileScope::global();
    unsafe {
        let result = run_code(&mut jit, text, ctx, ptr).unwrap();
        println!("result: {result:?}");
    };

    // let str = LispStr::from_str("test").tag();
    // println!("start clone");
    // let val = str.clone();
    // println!("finish clone");
    Ok(())
}

unsafe fn run_code(
    jit: &mut JIT,
    code: &str,
    ctx: CompileScope<'_>,
    env: *const Environment,
) -> anyhow::Result<Vec<anyhow::Result<Object>>> {
    unsafe {
        // Pass the string to the JIT, and it returns a raw pointer to machine code.
        let node = elisp_parser().parse(code).unwrap();
        // println!("{node:?}");
        let vals = node
            .into_iter()
            .map(node_to_ir)
            .map(|expr| {
                let expr = expr?;
                let f = jit.compile_expr(&expr, &ctx).unwrap();
                let code_fn = std::mem::transmute::<_, fn(*const Environment) -> Object>(f);
                // And now we can call it!
                let value = code_fn(env as *const Environment);
                let result = value;
                println!("result: {result:?}");
                Ok(result)
            })
            .collect::<Vec<_>>();
        Ok(vals)
    }
}
