mod error;
mod eval;
mod lexer;
mod log;
mod parser;

use std::env;
use std::fs;
use std::str;

use unicode_segmentation::UnicodeSegmentation;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let file_bytes = fs::read(filename).unwrap_or_else(|_| panic!("Couldn't read {}", &args[1]));
    let file_utf8 = str::from_utf8(&file_bytes).unwrap();
    let file_unicode = UnicodeSegmentation::graphemes(file_utf8, true).collect::<Vec<&str>>();

    let tokens: &mut Vec<lexer::Tok> = &mut Vec::new();
    lexer::tokenize(tokens, &file_unicode, true, true);
    let ast_nodes = parser::parse(tokens, true, true);
    println!("{}", ast_nodes.len());
}
