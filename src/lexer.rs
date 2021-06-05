use crate::eval;

// Kind is the sum type of all possible types
// of tokens in an Ink program
type Kind = Token;

#[derive(Debug)]
enum Token {
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

// pub fn Tokenize (unbuffered: )
