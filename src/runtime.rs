use crate::error;
use crate::eval;
use crate::log;
use std::sync::{
    atomic::{AtomicI32, Ordering::SeqCst},
    Arc, Mutex, RwLock,
};
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::time::{sleep, Duration};

pub const functions: &'static [&'static str] = &["out", "time", "wait", "string"];

impl eval::Context {
    // GoInk: load_environment loads all builtins (functions and constants) to a given Context.
    pub fn load_environment(&self) {
        let mut frame = self.frame.write().unwrap();
        for f in functions {
            frame.set(
                f.to_string(),
                eval::Value::NativeFunctionValue(eval::NativeFunctionValue {
                    name: f.to_string(),
                }),
            )
        }
    }

    pub fn native_function(
        &self,
        name: String,
        in_values: Vec<eval::Value>,
    ) -> Result<eval::Value, error::Err> {
        match name.as_str() {
            // "load" => ink_load

            // // interfaces
            // "args" => inkArgs,
            // "in"=> inkIn,
            "out" => return self.ink_out(in_values),
            // "dir"=> inkDir,
            // "make"=> inkMake,
            // "stat"=>inkStat,
            // "read"=> inkRead,
            // "write"=> inkWrite,
            // "delete"=> inkDelete,
            // "listen" => return self.ink_listen(in_values),
            // "req"=> inkReq,
            // "rand"=> inkRand,
            // "urand"=> inkUrand,
            "time" => return self.ink_time(in_values),
            "wait" => return self.ink_wait(in_values),
            // "exec"=> inkExec,
            // "env"=> inkEnv,
            // "exit"=> inkExit,
            // // math
            // "sin"=> inkSin,
            // "cos"=> inkCos,
            // "asin"=> inkAsin,
            // "acos"=> inkAcos,
            // "pow"=> inkPow,
            // "ln"=> inkLn,
            // "floor"=> inkFloor,
            // // type conversions
            "string" => return self.ink_string(in_values),
            // "number"=> inkNumber,
            // "point"=> inkPoint,
            // "char"=> inkChar,
            // // introspection
            // "type"=> inkType,
            // "len"=> inkLen,
            // "keys"=> inkKeys,
            _ => panic!("TODO"),
        }
    }

    fn ink_out(&self, in_values: Vec<eval::Value>) -> Result<eval::Value, error::Err> {
        if in_values.len() >= 1 {
            if let eval::Value::StringValue(s) = in_values[0].clone() {
                print!("{}", String::from_utf8(s).unwrap());
                return Ok(eval::NULL);
            }
        }
        return Err(error::Err {
            reason: error::ERR_RUNTIME,
            message: "out() takes 1 string argument".to_string(),
        });
    }

    fn ink_time(&self, in_values: Vec<eval::Value>) -> Result<eval::Value, error::Err> {
        let start = SystemTime::now();
        let since_the_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards");
        let in_ms = since_the_epoch.as_secs() as f64 * 1000.0
            + since_the_epoch.subsec_nanos() as f64 / 1_000_000.0;
        return Ok(eval::Value::NumberValue(in_ms));
    }

    fn ink_wait(&self, in_values: Vec<eval::Value>) -> Result<eval::Value, error::Err> {
        if in_values.len() < 2 {
            return Err(error::Err {
                reason: error::ERR_RUNTIME,
                message: "wait() takes 2 arguments".to_string(),
            });
        }
        let secs: u64;
        if let eval::Value::NumberValue(n) = in_values[0] {
            secs = n as u64 * 1000
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
        let listeners = &self.engine.write().unwrap().listeners;
        listeners.fetch_add(1, SeqCst);
        drop(listeners);

        let sender = self.event_channel.0.clone();
        tokio::spawn(async move {
            sleep(Duration::from_millis(secs)).await;
            println!("{}", in_values[1].clone());
            sender.send(eval::Message::InkFunctionCallback((
                in_values[1].clone(),
                false,
                vec![],
            )));
        });
        Ok(eval::NULL)
    }
    fn ink_string(&self, in_values: Vec<eval::Value>) -> Result<eval::Value, error::Err> {
        if in_values.len() < 1 {
            return Err(error::Err {
                reason: error::ERR_RUNTIME,
                message: "string() takes 1 argument".to_string(),
            });
        }
        match &in_values[0] {
            eval::Value::StringValue(s) => Ok(eval::Value::StringValue(s.clone())),
            eval::Value::NumberValue(n) => Ok(eval::Value::StringValue(
                (&eval::nv_to_s(*n)).as_bytes().to_vec(),
            )),
            eval::Value::BooleanValue(b) => {
                if *b {
                    Ok(eval::Value::StringValue("true".as_bytes().to_vec()))
                } else {
                    Ok(eval::Value::StringValue("false".as_bytes().to_vec()))
                }
            }
            eval::Value::NullValue(_) => Ok(eval::Value::StringValue("()".as_bytes().to_vec())),
            eval::Value::CompositeValue(cv) => Ok(eval::Value::StringValue(
                eval::value_table_to_string(&cv).as_bytes().to_vec(),
            )),
            eval::Value::FunctionValue(_) | eval::Value::NativeFunctionValue(_) => {
                Ok(eval::Value::StringValue("(function)".as_bytes().to_vec()))
            }
            _ => Ok(eval::Value::StringValue(vec![])),
        }
    }
}
