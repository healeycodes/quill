# ✒️ Quill

Quill is an in-progress Rust port of [Ink](https://github.com/thesephist/ink) (a minimal programming language inspired by modern JavaScript and Go, with functional style). It uses Tokio async/await for an experimental event loop and passes callbacks via Tokio channels. The variable/function naming, as well as the code organization, tries to stick closely to the Go project. This way, patches that are added to Ink and be ported here. 

<br>

_This project is paused for now as I've got everything out of it that I wanted to. However, pull-requests/fixes are welcome and I will thoughtfully engage with them._

<br>

An example program that Quill can handle:

```ink
log := msg => out(string(msg) + char(10))

` vars `
a := 'Hello, World!'
log(a)
a := true
log(a)

` lists `
b := [1, 2, 3]
log(b)

` recursive via tail call optimization `
factorial := n => n :: {
    0 -> 1
    _ -> n * factorial(n-1)
}
factorial(5)

` event loop examples `
w := (t, c) => wait(t, () => log(c))
w(0.1, 'a')
w(0.2, 'b')
w(0.3, 'c')
w(0.35, 'd')
w(0.4, 'e')
w(0.45, 'f')

` this prints before the wait() calls `
log(string(time()))

` a map `
observation := {
    weather: 'Sunny',
    'observedAt': {
        time: time()
    }
}

log(observation.weather)

` composite value to_string `
log(observation.observedAt)
```

Outputs:

```
Hello, World!
true
{0: 1, 1: 2, 2: 3}
1624863950232.9634
Sunny
{time: 1624863950234.2705}
a
b
c
d
e
f
()
```

<br>

## Debug options

Are currently forced on. So you'll get output like you were running Ink with `--verbose` and see debug information for the lexer, parser, and a frame dump.

e.g.

```
debug: lex -> identifier 'log' [41:1]
debug: lex -> '(' [41:4]
debug: lex -> identifier 'observation' [41:5]
debug: lex -> '.' [41:16]
debug: lex -> identifier 'observedAt' [41:17]
-- snip --
debug: parse -> Call (Identifier 'w') on (Number 0.45, String f)
debug: parse -> Call (Identifier 'log') on (Call (Identifier 'string') on (Call (Identifier 'time') on ()))
-- snip --
debug: frame dump {
        a -> true
        observation -> {observedAt: {time: 1624863950234.2705}, weather: 'Sunny'}
        string -> Native Function (string)
        w -> ier 'log') on (Identifier 'c'))))..
        out -> Native Function (out)
        factorial -> > (Binary (Identifier 'n') '*' (Call (Identifier 'factorial') on (Binary (Identifier 'n') '-' (Number 1))))})..
        time -> Native Function (time)
        char -> Native Function (char)
        wait -> Native Function (wait)
        b -> {0: 1, 1: 2, 2: 3}
        log -> ' (Call (Identifier 'char') on (Number 10))))..
} -prnt-> *
```

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

<br>

## Current Bug 

There are issues with defining (e.g. changing indexes of lists and keys of composite values).

:building_construction::building_construction::building_construction:
