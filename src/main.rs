use torrent::{core, gc};

use crate::core::{
    compiler::jit::JIT,
    env::{DynamicEnv, Environment},
    object::Object,
    parser::{macro_expansion::expand_and_resolve_everything, parse_src},
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    gc::init();
    let text = std::fs::read_to_string("src/test.el")?;
    // let text = include_str!("test.el");
    let mut ctx = gc::GcContext::new();
    let env = Environment::new(&mut ctx);
    let mut jit = JIT::new(&env, &mut ctx);
    unsafe {
        let result = run_code(&mut jit, &text, &env, &mut ctx).unwrap();
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
    ctx: &mut gc::GcContext,
) -> anyhow::Result<Vec<anyhow::Result<Object>>> {
    unsafe {
        // Pass the string to the JIT, and it returns a raw pointer to machine code.
        match parse_src(code) {
            Ok(exprs) => {
                let env_ptr = env as *const Environment;
                let dynamic = DynamicEnv {
                    len: 0,
                    parent: std::ptr::null_mut(),
                    bindings: [],
                };
                let dynamic_ptr = &dynamic as *const DynamicEnv;

                for expr in exprs.iter() {
                    expand_and_resolve_everything(expr, env)?;
                    tracing::debug!("{:?}", expr);
                }

                let vals = exprs
                    .into_iter()
                    .map(|expr| {
                        let f = jit.compile_expr(&expr).unwrap();
                        let code_fn = std::mem::transmute::<
                            _,
                            unsafe extern "C" fn(
                                *const Environment,
                                *const DynamicEnv,
                                Object,
                            ) -> Object,
                        >(f);
                        // And now we can call it!
                        let value = code_fn(env_ptr, dynamic_ptr, crate::core::object::nil());
                        println!("result: {value:?}");
                        ctx.root(value)
                    })
                    .collect::<Vec<_>>();
                Ok(vals.iter().map(|root| Ok(*root.get(ctx))).collect())
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
