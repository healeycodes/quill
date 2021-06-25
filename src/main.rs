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
    rc::Rc,
    sync::{Arc, Barrier, Mutex},
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let eng = Rc::new(RefCell::new(eval::Engine {
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
    }));
    let mut ctx = eng.borrow_mut().create_context(&eng);
    let result = ctx.exec_path(file_path.to_string());

    println!("{}", result.unwrap());
}
