pub const ERR_UNKNOWN: i32 = 0;
pub const ERR_SYNTAX: i32 = 1;
pub const ERR_RUNTIME: i32 = 2;
pub const ERR_SYSTEM: i32 = 40;
pub const ERR_ASSERT: i32 = 100;

#[derive(Debug)]
pub struct Err {
    pub reason: i32,
    pub message: String,
}

impl Err {
    pub fn error(&self) -> &str {
        return &self.message;
    }
}
