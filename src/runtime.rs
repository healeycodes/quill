// use crate::error;
// use crate::eval;
// use crate::log;
// use std::{sync::Arc, RwLock};
// use tokio::time::{sleep, Duration};

// impl eval::NativeFunctionValue {}

// // GoInk: LoadEnvironment loads all builtins (functions and constants) to a given Context.
// fn load_environment(ctx: Arc<RwLock<eval::Context>>) {
//     load_func(ctx, "wait", ink_wait);
// }

// fn load_func(
//     ctx: Arc<RwLock<eval::Context>>,
//     name: String,
//     exec: fn(Arc<RwLock<eval::Context>>, Vec<eval::Value>) -> Result<eval::Value, error::Err>,
// ) {
//     ctx.borrow_mut().frame.borrow_mut().set(
//         name,
//         eval::Value::NativeFunctionValue(eval::NativeFunctionValue {
//             name: name,
//             exec: exec,
//             ctx: Arc::clone(&ctx),
//         }),
//     )
// }

// fn ink_wait(ctx: Arc<RwLock<eval::Context>>, in_val: Vec<eval::Value>) -> Result<Value, Err> {
//     if in_val.len() < 2 {
//         return Err(error::Err {
//             reason: error::ERR_RUNTIME,
//             message: "wait() takes 2 arguments".to_string(),
//         });
//     }

//     let secs: f64;
//     if let eval::Value::NumberValue(n) = in_val[0] {
//         secs = n
//     } else {
//         return Err(error::Err {
//             reason: error::ERR_RUNTIME,
//             message: format!(
//                 "first argument to wait() should be a number, but got {}",
//                 in_val[0]
//             ),
//         });
//     }

//     // GoInk: This is a bit tricky, since we don't want wait() to hold the eval_lock
//     // on the Context while we're waiting for the timeout, but do want to hold
//     // the main function from completing
//     let mut writable = self.engine.write().unwrap();
//     let mut listeners = writable.listeners.write().unwrap();
//     *listeners += 1;

//     let eng = self.engine.clone();
//     let context = ctx.clone();
//     tokio::spawn(async move {
//         sleep(Duration::from_millis(100)).await;

//         context.write().unwrap().exec_listener(|| {
//             let result = eval::eval_ink_function(in_val[1], false, vec![]);
//             if let Err(err) = result {
//                 if let error::Err { reason, message } = err {
//                     log::log_err_f(reason, messages: &[message])
//                 } else {
//                     context.lo
//                 }
//             }
//         });

//         let writable_eng = eng.write().unwrap();
//         let mut listeners = writable_eng.listeners.write().unwrap();
//         *listeners -= 1;
//     });
// }
