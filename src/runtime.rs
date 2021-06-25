use crate::error;
use crate::eval;
use std::{cell::RefCell, rc::Rc};

impl eval::NativeFunctionValue {}

// GoInk: LoadEnvironment loads all builtins (functions and constants) to a given Context.
fn load_environment(ctx: Rc<RefCell<eval::Context>>) {
    load_func(ctx, "wait", ink_wait);
}

fn load_func(
    ctx: Rc<RefCell<eval::Context>>,
    name: String,
    exec: fn(Rc<RefCell<eval::Context>>, Vec<eval::Value>) -> Result<eval::Value, error::Err>,
) {
    ctx.borrow_mut().frame.borrow_mut().set(
        name,
        eval::Value::NativeFunctionValue(eval::NativeFunctionValue {
            name: name,
            exec: exec,
            ctx: Rc::clone(&ctx),
        }),
    )
}
