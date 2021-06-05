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

    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    for c in contents.chars() {
        println!(">{}<", c);
    }
}
