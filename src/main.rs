#![allow(dead_code)]
mod parser;
mod error;
mod lexer;
mod eval;
mod log;

use std::env;
use std::fs;
use std::str;

use unicode_segmentation::UnicodeSegmentation;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let file_bytes = fs::read(filename).expect(&format!("Couldn't read {}", &args[1]));
    let file_utf8 = str::from_utf8(&file_bytes).unwrap();
    let file_unicode = UnicodeSegmentation::graphemes(file_utf8, true).collect::<Vec<&str>>();

    let tokens = lexer::tokenize(&file_unicode, true, true);

    // println!("{:#?}", tokens);
}
