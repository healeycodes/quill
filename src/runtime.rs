use crate::error;
use crate::eval;
use crate::log;
use std::{sync::Arc, sync::Mutex, sync::RwLock};
use tokio::time::{sleep, Duration};

impl eval::NativeFunctionValue {}

impl eval::Context {
    // GoInk: LoadEnvironment loads all builtins (functions and constants) to a given Context.
    fn load_environment(&self) {
        self.load_func("wait".to_string(), ink_wait);
    }

    // LoadFunc loads a single Rust-implemented function into a Context.
    fn load_func(
        &self,
        name: String,
        exec: fn(Arc<eval::Context>, Vec<eval::Value>) -> Result<eval::Value, error::Err>,
    ) {
        self.frame.write().unwrap().set(
            name,
            eval::Value::NativeFunctionValue(eval::NativeFunctionValue {
                name: name.clone(),
                exec: exec,
                ctx: Arc::new(*self),
            }),
        )
    }
}

fn ink_wait(ctx: Arc<eval::Context>, in_val: Vec<eval::Value>) -> Result<eval::Value, error::Err> {
    if in_val.len() < 2 {
        return Err(error::Err {
            reason: error::ERR_RUNTIME,
            message: "wait() takes 2 arguments".to_string(),
        });
    }

    let secs: f64;
    if let eval::Value::NumberValue(n) = in_val[0] {
        secs = n
    } else {
        return Err(error::Err {
            reason: error::ERR_RUNTIME,
            message: format!(
                "first argument to wait() should be a number, but got {}",
                in_val[0]
            ),
        });
    }

    // GoInk: This is a bit tricky, since we don't want wait() to hold the eval_lock
    // on the Context while we're waiting for the timeout, but do want to hold
    // the main function from completing
    let writable = ctx.engine.write().unwrap();
    let mut listeners = writable.listeners.write().unwrap();
    *listeners += 1;

    let eng = ctx.engine.clone();
    let context = ctx.clone();
    tokio::spawn(async move {
        sleep(Duration::from_millis(100)).await;

        let cb = move || {
            let result = eval::eval_ink_function(in_val[1], false, vec![]);
            if let Err(err) = result {
                if let error::Err { reason, message } = err {
                    log::log_err_f(reason, &[message])
                } else {
                    ctx.log_err(error::Err {
                        reason: error::ERR_ASSERT,
                        message: "Eval of an Ink node returned error not of type Err".to_string(),
                    });
                }
            }
        };
        context.exec_listener(&cb);

        let writable_eng = eng.write().unwrap();
        let mut listeners = writable_eng.listeners.write().unwrap();
        *listeners -= 1;
    });
    Ok(eval::NULL)
}
