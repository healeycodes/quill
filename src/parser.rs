use crate::error;
use crate::lexer;

pub enum Node {
    UnaryExprNode {
        operator: lexer::Kind,
        operand: Box<Node>,
        position: lexer::Position,
    },
    BinaryExprNode {
        operator: lexer::Kind,
        left_operand: Box<Node>,
        right_operand: Box<Node>,
        position: lexer::Position,
    },
    FunctionCallNode {
        function: Box<Node>,
        arguments: Vec<Node>,
    },
    MatchClauseNode {
        target: Box<Node>,
        expression: Box<Node>,
    },
    MatchExprNode {
        condition: Box<Node>,
        clauses: Vec<Node>, // []MatchClauseNode
        position: lexer::Position,
    },
    ExpressionListNode {
        expressions: Vec<Node>,
        position: lexer::Position,
    },
    EmptyIdentifierNode {
        position: lexer::Position,
    },
    IdentifierNode {
        val: String,
        position: lexer::Position,
    },
    NumberLiteralNode {
        val: f64,
        position: lexer::Position,
    },
    StringLiteralNode {
        val: String,
        position: lexer::Position,
    },
    BooleanLiteralNode {
        val: bool,
        position: lexer::Position,
    },
    ObjectLiteralNode {
        entries: Vec<Node>, // []ObjectEntryNode
        position: lexer::Position,
    },
    ObjectEntryNode {
        key: Box<Node>,
        val: Box<Node>,
        position: lexer::Position,
    },
    ListLiteralNode {
        vals: Vec<Node>,
        position: lexer::Position,
    },
    FunctionLiteralNode {
        arguments: Vec<Node>,
        body: Box<Node>,
        position: lexer::Position,
    },
}

fn guard_unexpected_input_end(tokens: &Vec<lexer::Tok>, idx: usize) -> Result<(), error::Err> {
    if idx >= tokens.len() {
        if tokens.len() > 0 {
            return Err(error::Err {
                reason: error::ERR_SYNTAX,
                message: format!(
                    "unexpected end of input at {}",
                    tokens[tokens.len() - 1].string()
                ),
            });
        }

        return Err(error::Err {
            reason: error::ERR_SYNTAX,
            message: format!("unexpected end of input"),
        });
    }

    return Ok(());
}

// GoInk(edited): Parse transforms a list of Tok (tokens) to Node (AST nodes).
// This implementation uses recursive descent parsing.
pub fn parse(tokens: &Vec<lexer::Tok>, fatal_error: bool, debug_parser: bool) -> Vec<Node> {
    let mut nodes: Vec<Node> = Vec::new();
    let mut idx = 0;
    let length = tokens.len();

    while idx < length {
        if matches!(tokens[idx].kind, lexer::Token::Separator) {
            // GoInk: this sometimes happens when the repl receives comment inputs
            idx += 1;
            continue;
        }

        let parsed_expression = parse_expression(&tokens[idx..]);
        let expr = parsed_expression.0;
        let incr = parsed_expression.1;
        let err = parsed_expression.2;

        idx += 1;
    }

    return nodes;
}

fn parse_expression(tokens: &[lexer::Tok]) -> (Box<Node>, i32, error::Err) {
    let null_node = Box::new(Node::UnaryExprNode {
        operator: lexer::Kind::AccessorOp,
        operand: Box::new(Node::EmptyIdentifierNode {
            position: lexer::Position { line: 1, col: 1 },
        }),
        position: lexer::Position { line: 1, col: 1 },
    });
    return (
        null_node,
        0,
        error::Err {
            message: String::new(),
            reason: -1,
        },
    );
}
