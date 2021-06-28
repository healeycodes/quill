# ✒️ Quill

Quill is an in-progress port of [Ink](https://github.com/thesephist/ink) (a minimal programming language inspired by modern JavaScript and Go, with functional style).

<br>

This project is paused for now as I've got everything out of it that I wanted to. However, pull-requests/fixes are welcome and I will thoughtfully engage with them.

<br>

:building_construction: Current bug :building_construction:

There are issues with defining (indexes of lists and keys of composite values)

<br>

## My motivations for this project

To learn:

- Rust (this is my first Rust project) <sub>[0]</sub>
- More about lexing, parsing, and evaluating an abstract syntax tree (AST)
- More about Ink's guts — how it's put together, where my Ink code comes alive, etc.

<sub>[0]</sub> this means the number of lines of code and the complexity of it is not in an ideal place. Call it a first draft.

<br>

## Things that are working

Lexing, parsing, and evaluation.

There is an experimental event loop which implements `wait()` to prove that it works. It uses Tokio async/await and channels for callbacks.

In terms of Ink's files, progress could also be measured like this:

- ink.go `5%`
- error.go `90%`
- eval.go `95%`
- lexer.go `100%`
- log.go `90%`
- parser.go `100%`
- runtime.go `2%`
