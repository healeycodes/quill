#![allow(dead_code)]
mod log;
mod eval;
mod error;
mod lexer;

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let contents = fs::read(filename).expect("Something went wrong reading the file");
    lexer::tokenize(&contents, true, true);
}
