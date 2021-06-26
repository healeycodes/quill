use crate::error;
use crate::eval;
use crate::log;
use std::{sync::{Arc, Mutex, RwLock, atomic::{AtomicI32, Ordering::SeqCst}}};
use tokio::time::{sleep, Duration};

impl eval::NativeFunctionValue {}

pub const functions: &'static [&'static str] = &["wait"];

impl eval::Context {
    // GoInk: load_environment loads all builtins (functions and constants) to a given Context.
    pub fn load_environment(&self) {
        let mut frame = self.frame.write().unwrap();
        for f in functions {
            frame.set(
                f.to_string(),
                eval::Value::NativeFunctionValue(eval::NativeFunctionValue { name: f.to_string() }),
            )
        }
    }

    pub fn native_function(
        &self,
        name: String,
        in_values: Vec<eval::Value>,
    ) -> Result<eval::Value, error::Err> {
        match name.as_str() {
            "wait" => return self.ink_wait(in_values),
            _ => panic!("TODO"),
        }
    }

    fn ink_wait(&self, in_values: Vec<eval::Value>) -> Result<eval::Value, error::Err> {
        if in_values.len() < 2 {
            return Err(error::Err {
                reason: error::ERR_RUNTIME,
                message: "wait() takes 2 arguments".to_string(),
            });
        }
        let secs: f64;
        if let eval::Value::NumberValue(n) = in_values[0] {
            secs = n
        } else {
            return Err(error::Err {
                reason: error::ERR_RUNTIME,
                message: format!(
                    "first argument to wait() should be a number, but got {}",
                    in_values[0]
                ),
            });
        }
        // GoInk: This is a bit tricky, since we don't want wait() to hold the eval_lock
        // on the Context while we're waiting for the timeout, but do want to hold
        // the main function from completing
        // let writable = self.engine.write().unwrap();
        // let mut listeners = writable.listeners.write().unwrap();
        // *listeners += 1;

        let send = self.channel.0.clone();
        self.engine.write().unwrap().listeners.fetch_add(1, SeqCst);
        tokio::spawn(async move {
            sleep(Duration::from_millis(100)).await;
            send.send(eval::Message::InkFunctionCallback((in_values[1].clone(), false, vec![])))
        });
        Ok(eval::NULL)
    }
}
