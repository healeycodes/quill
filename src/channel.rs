use crate::error;
use crate::eval;

pub enum Message {
    AssertErr(Err),
    Result(Result<eval::Value, error::Err>),
}
