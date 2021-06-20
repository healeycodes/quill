mod error;
mod eval;
mod lexer;
mod log;
mod parser;
use std::collections::HashMap;
use std::env;
use std::sync::{Arc, Barrier, Mutex};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let eng = eval::Engine {
        listeners: Arc::new(Barrier::new(0)),
        fatal_error: false,
        permissions: eval::PermissionsConfig {
            read: true,
            write: true,
            net: true,
            exec: true,
        },
        debug: eval::DebugConfig {
            lex: true,
            parse: true,
            dump: true,
        },
        eval_lock: Mutex::new(0),
        contexts: HashMap::new(),
    };
    let mut ctx = eng.create_context();
    let result = ctx.exec_path(file_path.to_string());

    println!("{:?}", result);
}
