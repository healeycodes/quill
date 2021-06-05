#![allow(dead_code)]
mod error;
mod eval;
mod lexer;

use std::env;
use std::fs;
use std::io::{self, Read};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = &args[1];

    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    for c in contents.chars() {
        println!(">{}<", c);
    }
}
