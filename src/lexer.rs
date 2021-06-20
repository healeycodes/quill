use crate::error;
use crate::eval;
use crate::log;
use std::fmt;

// GoInk: Kind is the sum type of all possible types
// of tokens in an Ink program
pub type Kind = Token;

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    pub line: i32,
    pub col: i32,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}:{}", &self.line, &self.col);
    }
}

// GoInk: Tok is the monomorphic struct representing all Ink program tokens
// in the lexer.
#[derive(Debug)]
pub struct Tok {
    pub kind: Kind,
    // GoInk: str and num are both present to implement Tok
    // as a monomorphic type for all tokens; will be zero
    // values often.
    pub str: String,
    pub num: f64,
    pub position: Position,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            Token::Identifier | Token::StringLiteral => {
                write!(f, "{} '{}' [{}]", &self.kind, &self.str, &self.position)
            }
            Token::NumberLiteral => write!(
                f,
                "{} '{}' [{}]",
                &self.kind,
                eval::n_to_s(self.num),
                &self.position
            ),
            _ => write!(f, "{} [{}]", &self.kind, &self.position),
        }
    }
}

struct LexerState<'a> {
    tokens: &'a mut Vec<Tok>,
    buf: &'a mut String,
    last_kind: &'a mut Token,
    line_no: i32,
    col_no: i32,
    debug_lexer: bool,
    fatal_error: bool,
}

fn simple_commit(tok: Tok, lexer_state: &mut LexerState) {
    if lexer_state.debug_lexer {
        log::log_debug(&[String::from("lex ->"), tok.to_string()])
    }
    *lexer_state.last_kind = tok.kind;
    lexer_state.tokens.push(tok);
}

fn simple_commit_char(kind: &Kind, lexer_state: &mut LexerState) {
    simple_commit(
        Tok {
            num: 0.0,
            str: String::new(),
            kind: *kind,
            position: Position {
                line: lexer_state.line_no,
                col: lexer_state.col_no,
            },
        },
        lexer_state,
    )
}

fn commit_clear(lexer_state: &mut LexerState) {
    if lexer_state.buf.is_empty() {
        // GoInk: no need to commit empty token
        return;
    }

    let cbuf = lexer_state.buf.clone();
    *lexer_state.buf = String::new();
    match cbuf.as_str() {
        "true" => simple_commit_char(&Token::TrueLiteral, lexer_state),
        "false" => simple_commit_char(&Token::FalseLiteral, lexer_state),
        _ => {
            if cbuf.chars().next().unwrap().is_digit(10) {
                let f: f64 = match cbuf.parse::<f64>() {
                    Ok(f) => f,
                    Err(err) => {
                        let e = error::Err {
                            reason: error::ERR_SYNTAX,
                            message: format!(
                                "parsing error in number at {}:{}, {}",
                                lexer_state.line_no, lexer_state.col_no, err
                            ),
                        };
                        if lexer_state.fatal_error {
                            log::log_err(e.reason, &[e.message])
                        } else {
                            log::log_safe_err(e.reason, &[e.message])
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
                            line: lexer_state.line_no,
                            col: lexer_state.col_no - cbuf.char_indices().count() as i32,
                        },
                    },
                    lexer_state,
                )
            } else {
                simple_commit(
                    Tok {
                        str: cbuf.to_string(),
                        num: 0.0,
                        kind: Token::Identifier,
                        position: Position {
                            line: lexer_state.line_no,
                            col: lexer_state.col_no - cbuf.char_indices().count() as i32,
                        },
                    },
                    lexer_state,
                )
            }
        }
    }
}

fn commit(tok: Tok, lexer_state: &mut LexerState) {
    commit_clear(lexer_state);
    simple_commit(tok, lexer_state)
}

fn commit_char(kind: Kind, lexer_state: &mut LexerState) {
    commit(
        Tok {
            str: String::from(""),
            num: 0.0,
            kind,
            position: Position {
                line: lexer_state.line_no,
                col: lexer_state.col_no,
            },
        },
        lexer_state,
    )
}

fn ensure_separator(lexer_state: &mut LexerState) {
    commit_clear(lexer_state);
    match lexer_state.last_kind {
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
            // GoInk: do nothing
            {}
        _ => commit_char(Token::Separator, lexer_state),
    }
}

fn match_new_line(s: &str) -> bool {
    s == "\n" || s == "\r" || s == "\r\n"
}

pub fn tokenize(tokens: &mut Vec<Tok>, source: &[&str], fatal_error: bool, debug_lexer: bool) {
    let lexer_state = &mut LexerState {
        tokens,
        buf: &mut String::new(),
        last_kind: &mut Token::Separator,
        line_no: 1,
        col_no: 1,
        fatal_error,
        debug_lexer,
    };

    let mut strbuf = String::new();
    let mut strbuf_start_line = 0;
    let mut strbuf_start_col = 0;
    let mut in_string_literal = false;

    let mut source_pos = 0;
    if source.len() > 2 && source[..2] == ["#", "!"] {
        // GoInk: shebang-style ignored line, keep taking until EOL
        while source_pos < source.len() {
            source_pos += 1;
            if match_new_line(source[source_pos]) {
                break;
            }
        }
        lexer_state.line_no += 1;
    }
    while source_pos < source.len() {
        let character = source[source_pos];
        if character == "'" {
            if in_string_literal {
                commit(
                    Tok {
                        str: strbuf.clone(),
                        num: 0.0,
                        kind: Token::StringLiteral,
                        position: Position {
                            line: strbuf_start_line,
                            col: strbuf_start_col,
                        },
                    },
                    lexer_state,
                )
            } else {
                strbuf = String::from("");
                strbuf_start_line = lexer_state.line_no;
                strbuf_start_col = lexer_state.col_no;
            }
            in_string_literal = !in_string_literal;
        } else if in_string_literal {
            if match_new_line(character) {
                lexer_state.line_no += 1;
                lexer_state.col_no = 0;
                strbuf.push_str(character);
            } else if character == "\\" {
                // GoInk: backslash escapes like in most other languages,
                // so just consume whatever the next char is into
                // the current string buffer
                source_pos += 1;
                if source_pos == source.len() {
                    break;
                }
                strbuf.push_str(character);
                lexer_state.col_no += 1;
            } else {
                strbuf.push_str(character);
            }
        } else if character == "`" {
            source_pos += 1;
            if source_pos == source.len() {
                break;
            }
            let mut next_char = source[source_pos];

            if next_char == "`" {
                // GoInk: single-line comment, keep taking until EOL
                while source_pos < source.len() && !match_new_line(next_char) {
                    source_pos += 1;
                    next_char = source[source_pos];
                }

                ensure_separator(lexer_state);
                lexer_state.line_no += 1;
                lexer_state.col_no = 0;
            } else {
                // GoInk: multi-line block comment, keep taking until end of block
                while source_pos < source.len() && next_char != "`" {
                    source_pos += 1;
                    next_char = source[source_pos];

                    if match_new_line(next_char) {
                        lexer_state.line_no += 1;
                        lexer_state.col_no = 0;
                    }
                    lexer_state.col_no += 1;
                }
            }
        } else if match_new_line(character) {
            ensure_separator(lexer_state);
            lexer_state.line_no += 1;
            lexer_state.col_no = 0;
        // unicode.IsSpace TODO: unimplemented check for \f, 0x85 (NEL), and 0xA0 (NBSP)
        } else if character == " " || character == "\t" || character == "\n" || character == "\r" {
            commit_clear(lexer_state);
        } else if character == "_" {
            commit_char(Token::EmptyIdentifier, lexer_state)
        } else if character == "~" {
            commit_char(Token::NegationOp, lexer_state)
        } else if character == "+" {
            commit_char(Token::AddOp, lexer_state)
        } else if character == "*" {
            commit_char(Token::MultiplyOp, lexer_state)
        } else if character == "/" {
            commit_char(Token::DivideOp, lexer_state)
        } else if character == "%" {
            commit_char(Token::ModulusOp, lexer_state)
        } else if character == "&" {
            commit_char(Token::LogicalAndOp, lexer_state)
        } else if character == "|" {
            commit_char(Token::LogicalOrOp, lexer_state)
        } else if character == "^" {
            commit_char(Token::LogicalXorOp, lexer_state)
        } else if character == "<" {
            commit_char(Token::LessThanOp, lexer_state)
        } else if character == ">" {
            commit_char(Token::GreaterThanOp, lexer_state)
        } else if character == "," {
            commit_char(Token::Separator, lexer_state)
        } else if character == "." {
            // GoInk: only non-AccessorOp case is [Number token] . [Number],
            // so we commit and bail early if the buf is empty or contains
            // a clearly non-numeric token. Note that this means all numbers
            // must start with a digit. i.e. .5 is not 0.5 but a syntax error.
            // This is the case since we don't know what the last token was,
            // and I think streaming parse is worth the tradeoffs of losing
            // that context.
            let mut committed = false;
            for c in lexer_state.buf.chars() {
                if !c.is_digit(10) {
                    commit_char(Token::AccessorOp, lexer_state);
                    committed = true;
                    break;
                }
            }
            if !committed {
                if lexer_state.buf.is_empty() {
                    commit_char(Token::AccessorOp, lexer_state);
                } else {
                    lexer_state.buf.push('.')
                }
            }
        } else if character == ":" {
            source_pos += 1;
            if source_pos == source.len() {
                break;
            }
            let next_char = source[source_pos];

            lexer_state.col_no += 1;
            if next_char == "=" {
                commit_char(Token::DefineOp, lexer_state)
            } else if next_char == ":" {
                commit_char(Token::MatchColon, lexer_state)
            } else {
                // GoInk: key is parsed as expression, so make sure
                // we mark expression end (Separator)
                ensure_separator(lexer_state);
                commit_char(Token::KeyValueSeparator, lexer_state);
                source_pos -= 1;
            }
        } else if character == "=" {
            source_pos += 1;
            if source_pos == source.len() {
                break;
            }
            let next_char = source[source_pos];

            lexer_state.col_no += 1;
            if next_char == ">" {
                commit_char(Token::FunctionArrow, lexer_state);
            } else {
                commit_char(Token::EqualOp, lexer_state);
                source_pos -= 1;
            }
        } else if character == "-" {
            source_pos += 1;
            if source_pos == source.len() {
                break;
            }
            let next_char = source[source_pos];

            if next_char == ">" {
                commit_char(Token::CaseArrow, lexer_state)
            } else {
                commit_char(Token::SubtractOp, lexer_state);
                source_pos -= 1
            }
        } else if character == "(" {
            commit_char(Token::LeftParen, lexer_state)
        } else if character == ")" {
            ensure_separator(lexer_state);
            commit_char(Token::RightParen, lexer_state)
        } else if character == "[" {
            commit_char(Token::LeftBracket, lexer_state)
        } else if character == "]" {
            ensure_separator(lexer_state);
            commit_char(Token::RightBracket, lexer_state)
        } else if character == "{" {
            commit_char(Token::LeftBrace, lexer_state)
        } else if character == "}" {
            ensure_separator(lexer_state);
            commit_char(Token::RightBrace, lexer_state)
        } else {
            lexer_state.buf.push_str(character);
        }
        lexer_state.col_no += 1;
        source_pos += 1
    }
    ensure_separator(lexer_state);
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::UnaryExpr => "unary expression",
                Token::BinaryExpr => "binary expression",
                Token::MatchExpr => "match expression",
                Token::MatchClause => "match clause",
                Token::Identifier => "identifier",
                Token::EmptyIdentifier => "'_'",
                Token::FunctionCall => "function call",
                Token::NumberLiteral => "number literal",
                Token::StringLiteral => "string literal",
                Token::ObjectLiteral => "composite literal",
                Token::ListLiteral => "list composite literal",
                Token::FunctionLiteral => "function literal",
                Token::TrueLiteral => "'true'",
                Token::FalseLiteral => "'false'",
                Token::AccessorOp => "'.'",
                Token::EqualOp => "'='",
                Token::FunctionArrow => "'=>'",
                Token::KeyValueSeparator => "':'",
                Token::DefineOp => "':='",
                Token::MatchColon => "'::'",
                Token::CaseArrow => "'->'",
                Token::SubtractOp => "'-'",
                Token::NegationOp => "'~'",
                Token::AddOp => "'+'",
                Token::MultiplyOp => "'*'",
                Token::DivideOp => "'/'",
                Token::ModulusOp => "'%'",
                Token::GreaterThanOp => "'>'",
                Token::LessThanOp => "'<'",
                Token::LogicalAndOp => "'&'",
                Token::LogicalOrOp => "'|'",
                Token::LogicalXorOp => "'^'",
                Token::Separator => "','",
                Token::LeftParen => "'('",
                Token::RightParen => "')'",
                Token::LeftBracket => "'['",
                Token::RightBracket => "']'",
                Token::LeftBrace => "'{'",
                Token::RightBrace => "'}'",
            }
        )
    }
}
