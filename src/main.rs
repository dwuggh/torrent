pub mod core;
pub mod gc;

use crate::core::{
    compiler::jit::JIT,
    env::Environment,
    object::Object,
    parser::{macro_expansion::expand_and_resolve_everything, parse_src},
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    // gc::collector::init_gc();
    let text = std::fs::read_to_string("src/test.el")?;
    // let text = include_str!("test.el");
    let env = Environment::default();
    let mut jit = JIT::new(&env);
    unsafe {
        let result = run_code(&mut jit, &text, &env).unwrap();
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
    env: &Environment,
) -> anyhow::Result<Vec<anyhow::Result<Object>>> {
    unsafe {
        // Pass the string to the JIT, and it returns a raw pointer to machine code.
        match parse_src(code) {
            Ok(exprs) => {
                let env_ptr = env as *const Environment;

                for expr in exprs.iter() {
                    expand_and_resolve_everything(expr, env)?;
                    tracing::debug!("{:?}", expr);
                }

                let vals = exprs
                    .into_iter()
                    .map(|expr| {
                        let f = jit.compile_expr(&expr).unwrap();
                        let code_fn = std::mem::transmute::<_, fn(*const Environment) -> Object>(f);
                        // And now we can call it!
                        let value = code_fn(env_ptr);
                        let result = value;
                        println!("result: {result:?}");
                        Ok(result)
                    })
                    .collect::<Vec<_>>();
                Ok(vals)
            }
            Err(err) => {
                for e in err.iter() {
                    let span = e.span();
                    println!("{:?} at {}", e.reason(), e.span());
                    println!("{:?}", &code[span.start()..span.end()]);
                }
                panic!()
            }
        }
        // println!("{node:?}");
    }
}
