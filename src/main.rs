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

pub mod ast;
// mod compiler;
pub mod gc;
pub mod core;
pub mod runtime;

fn main() {}
