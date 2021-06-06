use crate::lexer;

// GoInk: Node represents an abstract syntax tree (AST) node in an Ink program.
pub trait Node {
    fn string(&self) -> String;
    fn position(&self) -> lexer::Position;
    // fn eval(&self, StackFrame, bool) -> Value;
}

// GoInk: a string representation of the Position of a given node,
// appropriate for an error message
fn poss(n: dyn Node) -> String {
	return n.Position().String()
}

 struct UnaryExprNode {
	operator: lexer::Kind,
	operand: &Node,
	position: lexer::Position
}

impl UnaryExprNode {
    fn string(&self) -> String {
        return format!("Unary {} ({})", self.operator, self.operand)
    }
    fn position(&self) -> lexer::Position {
        return self.position
    }
}

// GoInk: Parse transforms a stream of Tok (tokens) to Node (AST nodes).
// This implementation uses recursive descent parsing.
pub fn parse(tokens: &Vec<lexer::Token>, fatal_error: bool, debug_parser: bool) -> Box<dyn Node> {
    let mut nodes: Vec<dyn Node> = Vec::new();
    let mut idx = 0;
    let length = tokens.len();

    while idx < length {
        if tokens[idx].kind == lexer::Token::Separator {
            // GoInk: this sometimes happens when the repl receives comment inputs
			idx += 1;
			continue
        }
    }

    return Box::new(nodes);
}