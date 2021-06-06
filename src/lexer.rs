use crate::error;
use crate::eval;
use crate::log;

// Kind is the sum type of all possible types
// of tokens in an Ink program
type Kind = Token;

#[derive(Debug, Copy)]
pub enum Token {
    Separator,

    UnaryExpr,
    BinaryExpr,
    MatchExpr,
    MatchClause,

    Identifier,
    EmptyIdentifier,

    FunctionCall,

    NumberLiteral,
    StringLiteral,
    ObjectLiteral,
    ListLiteral,
    FunctionLiteral,

    TrueLiteral,
    FalseLiteral,

    // ambiguous operators and symbols
    AccessorOp,

    // =
    EqualOp,
    FunctionArrow,

    // :
    KeyValueSeparator,
    DefineOp,
    MatchColon,

    // -
    CaseArrow,
    SubtractOp,

    // single char, unambiguous
    NegationOp,
    AddOp,
    MultiplyOp,
    DivideOp,
    ModulusOp,
    GreaterThanOp,
    LessThanOp,

    LogicalAndOp,
    LogicalOrOp,
    LogicalXorOp,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

impl Clone for Token {
    fn clone(&self) -> Token {
        *self
    }
}

#[derive(Debug)]
struct Position {
    line: i32,
    col: i32,
}

impl Position {
    fn string(&self) -> String {
        return format!("{}:{}", &self.line, &self.col);
    }
}

// Tok is the monomorphic struct representing all Ink program tokens
// in the lexer.
#[derive(Debug)]
struct Tok {
    kind: Kind,
    // str and num are both present to implement Tok
    // as a monomorphic type for all tokens; will be zero
    // values often.
    str: String,
    num: f64,
    position: Position,
}

impl Tok {
    fn string(&self) -> String {
        match self.kind {
            Token::Identifier | Token::StringLiteral => {
                format!(
                    "{:?} '{}' [{}]",
                    &self.kind,
                    &self.str,
                    &self.position.string()
                )
            }
            Token::NumberLiteral => format!(
                "{:?} '{}' [{}]",
                &self.kind,
                eval::n_to_s(self.num),
                &self.position.string()
            ),
            _ => format!("{:?} [{}]", &self.kind, &self.position.string()),
        }
    }
}

fn simple_commit(tok: Tok, last_kind: &mut Token, tokens: &mut Vec<Token>, debug_lexer: bool) {
    if debug_lexer {
        log::log_debug("lex ->".to_string(), tok.string())
    }
    *last_kind = tok.kind;
    tokens.push(tok.kind);
}

fn simple_commit_char(
    kind: &Kind,
    last_kind: &mut Token,
    tokens: &mut Vec<Token>,
    debug_lexer: bool,
    line_no: i32,
    col_no: i32,
) {
    simple_commit(
        Tok {
            num: 0.0,
            str: String::new(),
            kind: *kind,
            position: Position {
                line: line_no,
                col: col_no,
            },
        },
        last_kind,
        tokens,
        debug_lexer,
    )
}

fn commit_clear(
    buf: &mut String,
    last_kind: &mut Token,
    tokens: &mut Vec<Token>,
    debug_lexer: bool,
    fatal_error: bool,
    line_no: i32,
    col_no: i32,
) {
    if buf == "" {
        // no need to commit empty token
        return;
    }

    let cbuf = buf.clone();
    *buf = String::new();
    match cbuf.as_str() {
        "true" => simple_commit_char(
            &Token::TrueLiteral,
            last_kind,
            tokens,
            debug_lexer,
            line_no,
            col_no,
        ),
        "false" => simple_commit_char(
            &Token::FalseLiteral,
            last_kind,
            tokens,
            debug_lexer,
            line_no,
            col_no,
        ),
        _ => {
            if cbuf.chars().nth(0).unwrap().is_digit(10) {
                let f: f64 = match cbuf.parse::<f64>() {
                    Ok(f) => f,
                    Err(err) => {
                        let e = error::Err {
                            reason: error::ERR_SYNTAX,
                            message: format!(
                                "parsing error in number at {}:{}, {}",
                                line_no,
                                col_no,
                                err.to_string()
                            ),
                        };
                        if fatal_error {
                            log::log_err(e.reason, &e.message)
                        } else {
                            log::log_safe_err(e.reason, &e.message)
                        }
                        0.0
                    }
                };
                simple_commit(
                    Tok {
                        str: String::new(),
                        num: f,
                        kind: Token::NumberLiteral,
                        position: Position {
                            line: line_no,
                            col: col_no - cbuf.char_indices().count() as i32,
                        },
                    },
                    last_kind,
                    tokens,
                    debug_lexer,
                )
            } else {
                simple_commit(
                    Tok {
                        str: cbuf.to_string(),
                        num: 0.0,
                        kind: Token::Identifier,
                        position: Position {
                            line: line_no,
                            col: col_no - cbuf.char_indices().count() as i32,
                        },
                    },
                    last_kind,
                    tokens,
                    debug_lexer,
                )
            }
        }
    }
}

fn commit(
    tok: Tok,
    buf: &mut String,
    last_kind: &mut Token,
    tokens: &mut Vec<Token>,
    debug_lexer: bool,
    fatal_error: bool,
    line_no: i32,
    col_no: i32,
) {
    commit_clear(
        buf,
        last_kind,
        tokens,
        debug_lexer,
        fatal_error,
        line_no,
        col_no,
    );
    simple_commit(tok, last_kind, tokens, debug_lexer)
}

fn commit_char(
    kind: Kind,
    buf: &mut String,
    last_kind: &mut Token,
    tokens: &mut Vec<Token>,
    debug_lexer: bool,
    fatal_error: bool,
    line_no: i32,
    col_no: i32,
) {
    commit(
        Tok {
            str: String::from(""),
            num: 0.0,
            kind: kind,
            position: Position {
                line: line_no,
                col: col_no,
            },
        },
        buf,
        last_kind,
        tokens,
        debug_lexer,
        fatal_error,
        line_no,
        col_no,
    )
}

fn ensure_separator(
    buf: &mut String,
    last_kind: &mut Token,
    tokens: &mut Vec<Token>,
    debug_lexer: bool,
    fatal_error: bool,
    line_no: i32,
    col_no: i32,
) {
    commit_clear(
        buf,
        last_kind,
        tokens,
        debug_lexer,
        fatal_error,
        line_no,
        col_no,
    );
    match last_kind {
        Token::Separator
        | Token::LeftParen
        | Token::LeftBracket
        | Token::LeftBrace
        | Token::AddOp
        | Token::SubtractOp
        | Token::MultiplyOp
        | Token::DivideOp
        | Token::ModulusOp
        | Token::NegationOp
        | Token::GreaterThanOp
        | Token::LessThanOp
        | Token::EqualOp
        | Token::DefineOp
        | Token::AccessorOp
        | Token::KeyValueSeparator
        | Token::FunctionArrow
        | Token::MatchColon
        | Token::CaseArrow =>
            // do nothing
            {}
        _ => commit_char(
            Token::Separator,
            buf,
            last_kind,
            tokens,
            debug_lexer,
            fatal_error,
            line_no,
            col_no,
        ),
    }
}

pub fn tokenize(source: &Vec<&str>, fatal_error: bool, debug_lexer: bool) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut buf = String::new();
    let mut strbuf = String::new();
    let mut strbuf_start_line = 0;
    let mut strbuf_start_col = 0;

    let last_kind = &mut Token::Separator;

    let mut line_no = 1;
    let mut col_no = 1;

    let mut in_string_literal = false;

    let mut source_pos = 0;
    if source.len() > 2 && source[..2] == ["#", "!"] {
        // shebang-style ignored line, keep taking until EOL
        while source[source_pos] != "\n" {
            source_pos += 1;
        }
        line_no += 1;
    }
    while source_pos < source.len() {
        let character = source[source_pos];
        match character {
            "\\" => {
                if in_string_literal {
                    commit(
                        Tok {
                            str: strbuf,
                            num: 0.0,
                            kind: Token::StringLiteral,
                            position: Position {
                                line: strbuf_start_line,
                                col: strbuf_start_col,
                            },
                        },
                        &mut buf,
                        last_kind,
                        &mut tokens,
                        debug_lexer,
                        fatal_error,
                        line_no,
                        col_no,
                    )
                } else {
                    strbuf = String::from("");
                    strbuf_start_line = line_no;
                    strbuf_start_col = col_no;
                }
                in_string_literal = !in_string_literal;
            }
        }
    }

    return tokens;
}
