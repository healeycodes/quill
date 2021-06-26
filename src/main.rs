mod error;
mod eval;
mod lexer;
mod log;
mod parser;
mod runtime;
use std::{
    cell::RefCell,
    collections::HashMap,
    env,
    sync::{Arc, Barrier, Mutex, RwLock, atomic::AtomicI32},
};

#[tokio::main]
async fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let eng = Arc::new(RwLock::new(eval::Engine {
        listeners: AtomicI32::new(0),
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
        eval_lock: Mutex::new(false),
        contexts: HashMap::new(),
    }));
    let mut ctx = eng.write().unwrap().create_context(&eng);
    let result = ctx.exec_path(file_path.to_string());

    println!("{}", result.await.unwrap());
}
