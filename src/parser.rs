use crate::lexer;

pub enum Node {
    UnaryExprNode {
        operator: lexer::Kind,
        operand: Box<Node>,
        position: lexer::Position,
    },
}

impl crate::parser::Node::UnaryExprNode {
    fn string(&self) -> String {
        format!("Unary {} ({})", self.operator, self.operand)
    }
}


// GoInk: Parse transforms a stream of Tok (tokens) to Node (AST nodes).
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
    }

    return nodes;
}
