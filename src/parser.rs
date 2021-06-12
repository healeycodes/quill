use crate::error;
use crate::lexer;
use crate::log;
use std::fmt;

struct UnaryExprNode {
    operator: lexer::Kind,
    operand: Box<Node>,
    position: lexer::Position,
}
impl fmt::Display for UnaryExprNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "Unary {:?} ({})", self.operator, self.operand);
    }
}

struct BinaryExprNode {
    operator: lexer::Kind,
    left_operand: Box<Node>,
    right_operand: Box<Node>,
    position: lexer::Position,
}
impl fmt::Display for BinaryExprNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(
            f,
            "Binary ({}) {:?} ({})",
            self.left_operand, self.operator, self.right_operand
        );
    }
}

struct FunctionCallNode {
    function: Box<Node>,
    arguments: Vec<Box<Node>>,
}
impl fmt::Display for FunctionCallNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(
            f,
            "Call ({}) on ({})",
            self.function,
            (*self.arguments)
                .into_iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
    }
}
impl Node for FunctionCallNode {
    fn position(&self) -> lexer::Position {
        return self.function.position();
    }
}

struct MatchClauseNode {
    target: Box<Node>,
    expression: Box<Node>,
}
impl fmt::Display for MatchClauseNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "Clause ({}) -> ({})", self.target, self.expression);
    }
}
impl Node for MatchClauseNode {
    fn position(&self) -> lexer::Position {
        return self.target.position();
    }
}

struct MatchExprNode {
    condition: Box<Node>,
    clauses: Vec<Box<MatchClauseNode>>,
    position: lexer::Position,
}
impl fmt::Display for MatchExprNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(
            f,
            "Match on ({}) to {{{}}}",
            self.condition,
            (*self.clauses)
                .into_iter()
                .map(|clause| clause.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
    }
}

struct ExpressionListNode {
    expressions: Vec<Box<Node>>,
    position: lexer::Position,
}
impl fmt::Display for ExpressionListNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(
            f,
            "Expression List ({})",
            (*self.expressions)
                .into_iter()
                .map(|expr| expr.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
    }
}

struct EmptyIdentifierNode {
    position: lexer::Position,
}
impl fmt::Display for EmptyIdentifierNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct IdentifierNode {
    val: String,
    position: lexer::Position,
}
impl fmt::Display for IdentifierNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct NumberLiteralNode {
    val: f64,
    position: lexer::Position,
}
impl fmt::Display for NumberLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct StringLiteralNode {
    val: String,
    position: lexer::Position,
}
impl fmt::Display for StringLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct BooleanLiteralNode {
    val: bool,
    position: lexer::Position,
}
impl fmt::Display for BooleanLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct ObjectLiteralNode {
    entries: Vec<ObjectEntryNode>,
    position: lexer::Position,
}
impl fmt::Display for ObjectLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct ObjectEntryNode {
    key: Box<Node>,
    val: Box<Node>,
    position: lexer::Position,
}
impl fmt::Display for ObjectEntryNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct ListLiteralNode {
    vals: Vec<Box<Node>>,
    position: lexer::Position,
}
impl fmt::Display for ListLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

struct FunctionLiteralNode {
    arguments: Vec<Box<Node>>,
    body: Box<Node>,
    position: lexer::Position,
}
impl fmt::Display for FunctionLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "");
    }
}

pub trait Node: fmt::Display {
    fn position(&self) -> lexer::Position;
}

macro_rules! impl_node {
    ($($t:ty),+) => {
        $(impl Node for $t {
            fn position(&self) -> lexer::Position {
                return self.position;
            }
        })+
    }
}

impl_node!(
    UnaryExprNode,
    BinaryExprNode,
    MatchExprNode,
    ExpressionListNode,
    EmptyIdentifierNode,
    IdentifierNode,
    NumberLiteralNode,
    StringLiteralNode,
    BooleanLiteralNode,
    ObjectEntryNode,
    ObjectLiteralNode,
    ListLiteralNode,
    FunctionLiteralNode
);

fn guard_unexpected_input_end(tokens: &[lexer::Tok], idx: usize) -> Result<(), error::Err> {
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

// GoInk: Parse transforms a list of Tok (tokens) to Node (AST nodes).
// This implementation uses recursive descent parsing.
pub fn parse(tokens: &Vec<lexer::Tok>, fatal_error: bool, debug_parser: bool) -> Vec<&Node> {
    let mut nodes: Vec<&Node> = Vec::new();
    let mut idx = 0;
    let length = tokens.len();

    while idx < length {
        if matches!(tokens[idx].kind, lexer::Token::Separator) {
            // GoInk: this sometimes happens when the repl receives comment inputs
            idx += 1;
            continue;
        }

        let (expr, incr) = parse_expression(&tokens[idx..]);
        idx += 1;

        match expr {
            Err(ref e) => match e.reason {
                error::ERR_UNKNOWN => log::log_err_f(
                    error::ERR_ASSERT,
                    &[format!("err raised that was not of Err type -> {:?}", e)],
                ),
                _ => {
                    if fatal_error {
                        log::log_err(e.reason, &[e.message.clone()])
                    } else {
                        log::log_safe_err(e.reason, &[e.message.clone()])
                    }
                }
            },
            _ => {}
        }

        if debug_parser {
            log::log_debug(&[format!("parse -> {}", expr.unwrap())])
        }
    }

    return nodes;
}

fn get_op_priority(t: &lexer::Tok) -> isize {
    // GoInk: higher == greater priority
    match t.kind {
        lexer::Token::AccessorOp => 100,
        lexer::Token::ModulusOp => 80,
        lexer::Token::MultiplyOp | lexer::Token::DivideOp => 50,
        lexer::Token::AddOp | lexer::Token::SubtractOp => 40,
        lexer::Token::GreaterThanOp | lexer::Token::LessThanOp | lexer::Token::EqualOp => 30,
        lexer::Token::LogicalAndOp => 20,
        lexer::Token::LogicalXorOp => 15,
        lexer::Token::LogicalOrOp => 10,
        lexer::Token::DefineOp => 0,
        _ => -1,
    }
}

fn is_binary_op(t: &lexer::Tok) -> bool {
    match t.kind {
        lexer::Token::AddOp
        | lexer::Token::SubtractOp
        | lexer::Token::MultiplyOp
        | lexer::Token::DivideOp
        | lexer::Token::ModulusOp
        | lexer::Token::LogicalAndOp
        | lexer::Token::LogicalOrOp
        | lexer::Token::LogicalXorOp
        | lexer::Token::GreaterThanOp
        | lexer::Token::LessThanOp
        | lexer::Token::EqualOp
        | lexer::Token::DefineOp
        | lexer::Token::AccessorOp => true,
        _ => false,
    }
}

fn parse_binary_expression(
    left_operand: Box<Node>,
    operator: &lexer::Tok,
    tokens: &[lexer::Tok],
    previous_priority: isize,
) -> (Result<Box<Node>, error::Err>, usize) {
    // return Ok((
    //     Box::new(EmptyIdentifierNode {
    //         position: lexer::Position { line: 1, col: 1 },
    //     }),
    //     0,
    // ));

    let (right_atom, mut idx) = parse_atom(&tokens);
    match right_atom {
        Err(right_atom) => return (Err(right_atom), 0),
        _ => {}
    }
    let mut incr = 0;

    let mut ops: Vec<&lexer::Tok> = Vec::new();
    let mut nodes: Vec<Box<Node>> = Vec::new();
    ops.push(operator);
    nodes.push(left_operand);
    nodes.push(right_atom.unwrap());

    // GoInk: build up a list of binary operations, with tree nodes
    // where there are higher-priority binary ops
    while tokens.len() > idx && is_binary_op(&tokens[idx]) {
        if previous_priority >= get_op_priority(&tokens[idx]) {
            // GoInk: Priority is lower than the calling function's last op,
            //  so return control to the parent binary op
            break;
        } else if get_op_priority(ops.last().unwrap()) >= get_op_priority(&tokens[idx]) {
            // GoInk: Priority is lower than the previous op (but higher than parent),
            // so it's ok to be left-heavy in this tree
            ops.push(&tokens[idx]);
            idx += 1;

            let err = guard_unexpected_input_end(&tokens, idx);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
            }

            let (right_atom, incr) = parse_atom(&tokens[idx..]);
            match right_atom {
                Err(right_atom) => return (Err(right_atom), 0),
                _ => {}
            }

            nodes.push(right_atom.unwrap());
            idx += incr;
        } else {
            let err = guard_unexpected_input_end(tokens, idx);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
            }

            // GoInk: Priority is higher than previous ops,
            // so make it a right-heavy tree
            let (subtree, incr) = parse_binary_expression(
                *nodes.last().unwrap(),
                &tokens[idx],
                &tokens[idx + 1..],
                get_op_priority(ops.last().unwrap()),
            );
            match subtree {
                Err(subtree) => return (Err(subtree), 0),
                _ => {}
            }

            nodes[nodes.len() - 1] = subtree.unwrap();
            idx += incr + 1;
        }
    }

    // GoInk: ops, nodes -> left-biased binary expression tree
    let mut tree = nodes[0];
    nodes.drain(0..1);
    while ops.len() > 0 {
        tree = Box::new(BinaryExprNode {
            operator: ops[0].kind,
            left_operand: tree,
            right_operand: nodes[0],
            position: ops[0].position,
        });
        nodes.drain(0..1);
    }

    return (Ok(tree), idx);
}

fn parse_expression(tokens: &[lexer::Tok]) -> (Result<Box<Node>, error::Err>, usize) {
    let mut idx = 0;

    let mut consume_dangling_separator = || {
        // GoInk: bounds check in case parse_expression called at some point
        // consumed end token
        if idx < tokens.len() && tokens[idx].kind == lexer::Token::Separator {
            idx += 1;
        }
    };

    let (atom, incr) = parse_atom(&tokens[idx..]);
    match atom {
        Err(atom) => return (Err(atom), 0),
        _ => {}
    }
    idx += incr;

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    let next_tok = &tokens[idx];
    idx += 1;

    match next_tok.kind {
        // GoInk: consuming dangling separator
        lexer::Token::Separator => return (Ok(atom.unwrap()), idx),
        // GoInk:these belong to the parent atom that contains this expression,
        // so return without consuming token (idx - 1)
        lexer::Token::RightParen | lexer::Token::KeyValueSeparator | lexer::Token::CaseArrow => {
            return (Ok(atom.unwrap()), idx - 1)
        }
        lexer::Token::AddOp
        | lexer::Token::SubtractOp
        | lexer::Token::MultiplyOp
        | lexer::Token::DivideOp
        | lexer::Token::ModulusOp
        | lexer::Token::LogicalAndOp
        | lexer::Token::LogicalOrOp
        | lexer::Token::LogicalXorOp
        | lexer::Token::GreaterThanOp
        | lexer::Token::LessThanOp
        | lexer::Token::EqualOp
        | lexer::Token::DefineOp
        | lexer::Token::AccessorOp => {
            let (bin_expr, incr) =
                parse_binary_expression(atom.unwrap(), next_tok, &tokens[idx..], -1);
            match bin_expr {
                Err(bin_expr) => return (Err(bin_expr), 0),
                _ => {}
            }
            idx += incr;

            // GoInk: Binary expressions are often followed by a match
            if idx < tokens.len() && tokens[idx].kind == lexer::Token::MatchColon {
                let colon_pos = tokens[idx].position;
                idx += 1; // GoInk: MatchColon

                let (clauses, incr) = parse_match_body(&tokens[idx..]);
                match clauses {
                    Err(clauses) => return (Err(clauses), 0),
                    _ => {}
                }
                idx += incr;

                consume_dangling_separator();
                return (
                    Ok(Box::new(MatchExprNode {
                        condition: bin_expr.unwrap(),
                        clauses: clauses.unwrap(),
                        position: colon_pos,
                    })),
                    idx,
                );
            }

            consume_dangling_separator();
            return (Ok(bin_expr.unwrap()), idx);
        }
        lexer::Token::MatchColon => {
            let (clauses, incr) = parse_match_body(&tokens[idx..]);
            match clauses {
                Err(clauses) => return (Err(clauses), 0),
                _ => {}
            }

            consume_dangling_separator();
            return (
                Ok(Box::new(MatchExprNode {
                    condition: atom.unwrap(),
                    clauses: clauses.unwrap(),
                    position: next_tok.position,
                })),
                idx,
            );
        }
        _ => {
            return (
                Err(error::Err {
                    message: format!("unexpected token {:?} following an expression", next_tok),
                    reason: error::ERR_SYNTAX,
                }),
                0,
            )
        }
    }
}

fn parse_atom(tokens: &[lexer::Tok]) -> (Result<Box<Node>, error::Err>, usize) {
    let mut err = guard_unexpected_input_end(tokens, 0);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    let tok = &tokens[0];
    let mut idx = 1;

    if tok.kind == lexer::Token::NegationOp {
        let (atom, idx) = parse_atom(&tokens[idx..]);
        match atom {
            Err(atom) => return (Err(atom), 0),
            _ => {}
        }
        return (
            Ok(Box::new(UnaryExprNode {
                operator: tok.kind,
                operand: atom.unwrap(),
                position: tok.position,
            })),
            idx + 1,
        );
    }

    err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    let mut atom: Box<Node>;
    match tok.kind {
        lexer::Token::NumberLiteral => {
            return (
                Ok(Box::new(NumberLiteralNode {
                    val: tok.num,
                    position: tok.position,
                })),
                idx,
            );
        }
        lexer::Token::StringLiteral => {
            return (
                Ok(Box::new(StringLiteralNode {
                    val: tok.str.clone(),
                    position: tok.position,
                })),
                idx,
            );
        }
        lexer::Token::TrueLiteral => {
            return (
                Ok(Box::new(BooleanLiteralNode {
                    val: true,
                    position: tok.position,
                })),
                idx,
            );
        }
        lexer::Token::FalseLiteral => {
            return (
                Ok(Box::new(BooleanLiteralNode {
                    val: false,
                    position: tok.position,
                })),
                idx,
            );
        }
        lexer::Token::Identifier => {
            match tokens[idx].kind {
                lexer::Token::FunctionArrow => {
                    let (_atom, _idx) = parse_function_literal(tokens);
                    match _atom {
                        Err(_atom) => return (Err(_atom), 0),
                        _ => atom = _atom.unwrap(),
                    }
                    // GoInk : parse_atom should not consume trailing Separators, but
                    // parseFunctionLiteral does because it ends with expressions.
                    // so we backtrack one token.
                    idx -= 1
                }
                _ => {
                    atom = Box::new(IdentifierNode {
                        val: tok.str.clone(),
                        position: tok.position,
                    })
                } // GoInk: may be called as a function, so flows beyond
                  // switch block
            }
        }
        lexer::Token::EmptyIdentifier => {
            match tokens[idx].kind {
                lexer::Token::FunctionArrow => {
                    let parsed_function_literal = parse_function_literal(&tokens);
                    match parsed_function_literal {
                        (Err(atom), _) => return (Err(atom), 0),
                        (Ok(_atom), _idx) => {
                            // parse_atom should not consume trailing Separators, but
                            // parse_function_literal does because it ends with expressions.
                            // so we backtrack one token.
                            return (Ok(_atom), _idx - 1);
                        }
                    }
                }
                _ => {}
            }

            return (
                Ok(Box::new(EmptyIdentifierNode {
                    position: tok.position,
                })),
                idx,
            );
        }
        // GoInk: may be called as a function, so flows beyond
        // switch block
        lexer::Token::LeftParen => {
            // GoInk: grouped expression or function literal
            let mut exprs: Vec<Box<Node>> = Vec::new();
            while tokens[idx].kind != lexer::Token::RightParen {
                let (expr, incr) = parse_expression(&tokens[idx..]);
                match expr {
                    Err(expr) => return (Err(expr), 0),
                    _ => {}
                }

                idx += incr;
                exprs.push(expr.unwrap());

                err = guard_unexpected_input_end(tokens, idx);
                match err {
                    Err(err) => return (Err(err), 0),
                    _ => {}
                }
            }
            idx += 1; // GoInk: RightParen

            err = guard_unexpected_input_end(tokens, idx);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
            }

            if tokens[idx].kind == lexer::Token::FunctionArrow {
                let (_atom, _idx) = parse_function_literal(tokens);
                match _atom {
                    Err(_atom) => return (Err(_atom), 0),
                    _ => atom = _atom.unwrap(),
                }

                // GoInk: parse_atom should not consume trailing Separators, but
                // parse_function_literal does because it ends with expressions.
                // so we backtrack one token.
                idx -= 1;
            } else {
                atom = Box::new(ExpressionListNode {
                    expressions: exprs,
                    position: tok.position,
                })
            }
            // GoInk: may be called as a function, so flows beyond
            // switch block
        }
        lexer::Token::LeftBrace => {
            let mut entries: Vec<ObjectEntryNode> = Vec::new();

            while tokens[idx].kind != lexer::Token::RightBrace {
                let (key_expr, key_incr) = parse_expression(&tokens[idx..]);
                match key_expr {
                    Err(key_expr) => return (Err(key_expr), 0),
                    _ => {}
                }

                idx += key_incr;
                err = guard_unexpected_input_end(tokens, idx);
                match err {
                    Err(err) => return (Err(err), 0),
                    _ => {}
                }

                if tokens[idx].kind == lexer::Token::KeyValueSeparator {
                    idx += 1;
                } else {
                    return (
                        Err(error::Err {
                            reason: error::ERR_SYNTAX,
                            message: format!(
                                "expected {:?} after composite key, found {:?}",
                                lexer::Token::KeyValueSeparator,
                                tokens[idx]
                            ),
                        }),
                        0,
                    );
                }

                err = guard_unexpected_input_end(tokens, idx);
                match err {
                    Err(err) => return (Err(err), 0),
                    _ => {}
                }

                let (val_expr, val_incr) = parse_expression(&tokens[idx..]);
                match val_expr {
                    Err(val_expr) => return (Err(val_expr), 0),
                    Ok(val_expr) => {
                        // GoInk : Separator consumed by parse_expression
                        idx += val_incr;

                        // TODO: there must be a shorthand for this?
                        let key_expr = key_expr.unwrap();
                        let position = key_expr.position();
                        entries.push(ObjectEntryNode {
                            key: key_expr,
                            val: val_expr,
                            position: position,
                        });
                    }
                }

                err = guard_unexpected_input_end(tokens, idx);
                match err {
                    Err(err) => return (Err(err), 0),
                    _ => {}
                }
            }
            idx += 1; // GoInk: RightBrace

            return (
                Ok(Box::new(ObjectLiteralNode {
                    entries: entries,
                    position: tok.position,
                })),
                idx,
            );
        }
        lexer::Token::LeftBracket => {
            let mut vals: Vec<Box<Node>> = Vec::new();

            while tokens[idx].kind != lexer::Token::RightBracket {
                let (expr, incr) = parse_expression(&tokens[idx..]);
                match expr {
                    Err(expr) => return (Err(expr), 0),
                    Ok(expr) => {
                        idx += incr;
                        vals.push(expr);
                    }
                }

                let err = guard_unexpected_input_end(tokens, idx);
                match err {
                    Err(err) => return (Err(err), 0),
                    _ => {}
                }
            }
            idx += 1; // GoInk: RightBracket

            return (
                Ok(Box::new(ListLiteralNode {
                    vals: vals,
                    position: tok.position,
                })),
                idx,
            );
        }
        _ => {
            return (
                Err(error::Err {
                    reason: error::ERR_SYNTAX,
                    message: format!("unexpected start of atom, found {:?}", tok),
                }),
                0,
            );
        }
    };

    // GoInk : bounds check here because parse_expression may have consumed all tokens before this
    while idx < tokens.len() && tokens[idx].kind == lexer::Token::LeftParen {
        let incr: usize;
        match parse_function_call(atom, &tokens[idx..]) {
            (Err(_atom), _) => return (Err(_atom), 0),
            (Ok(_atom), _incr) => {
                atom = _atom;
                incr = _incr;
            }
        }
        idx += incr;

        let err = guard_unexpected_input_end(tokens, idx);
        match err {
            Err(err) => return (Err(err), 0),
            _ => {}
        }
    }

    return (Ok(atom), idx);
}

// GoInk: parses everything that follows MatchColon
// does not consume dangling separator -- that's for parse_expression
fn parse_match_body(
    tokens: &[lexer::Tok],
) -> (Result<Vec<Box<MatchClauseNode>>, error::Err>, usize) {
    let mut idx = 1; // GoInk: LeftBrace
    let mut clauses: Vec<Box<MatchClauseNode>> = Vec::new();

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    while tokens[idx].kind != lexer::Token::RightBrace {
        let (clause_node, incr) = parse_match_clause(&tokens[idx..]);
        match clause_node {
            Err(clause_node) => return (Err(clause_node), 0),
            _ => {}
        }
        idx += incr;

        clauses.push(clause_node.unwrap());

        let err = guard_unexpected_input_end(tokens, idx);
        match err {
            Err(err) => return (Err(err), 0),
            _ => {}
        }
    }
    idx += 1; // GoInk: RightBrace

    return (Ok(clauses), idx);
}

fn parse_match_call(tokens: &[lexer::Tok]) -> (Result<Box<MatchClauseNode>, error::Err>, usize) {
    let (atom, mut idx) = parse_expression(&tokens);
    match atom {
        Err(atom) => return (Err(atom), 0),
        _ => {}
    }

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    if tokens[idx].kind != lexer::Token::CaseArrow {
        return (
            Err(error::Err {
                reason: error::ERR_SYNTAX,
                message: format!(
                    "expected {:?}, but got {:?}",
                    lexer::Token::CaseArrow,
                    tokens[idx]
                ),
            }),
            0,
        );
    }
    idx += 1; // GoInk: CaseArrow

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    let (expr, incr) = parse_expression(&tokens[idx..]);
    match expr {
        Err(expr) => return (Err(expr), 0),
        _ => {}
    }
    idx += incr;

    return (
        Ok(Box::new(MatchClauseNode {
            target: atom.unwrap(),
            expression: expr.unwrap(),
        })),
        idx,
    );
}

fn parse_match_clause(tokens: &[lexer::Tok]) -> (Result<Box<MatchClauseNode>, error::Err>, usize) {
    let (atom, mut idx) = parse_expression(&tokens);
    match atom {
        Err(atom) => return (Err(atom), 0),
        _ => {}
    }

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    if tokens[idx].kind != lexer::Token::CaseArrow {
        return (
            Err(error::Err {
                reason: error::ERR_SYNTAX,
                message: format!(
                    "expected {:?}, but got {:?}",
                    lexer::Token::CaseArrow,
                    tokens[idx]
                ),
            }),
            0,
        );
    }
    idx += 1; // CaseArrow

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    let (expr, incr) = parse_expression(&tokens[idx..]);
    match expr {
        Err(expr) => return (Err(expr), 0),
        _ => {}
    }
    idx += incr;

    return (
        Ok(Box::new(MatchClauseNode {
            target: atom.unwrap(),
            expression: expr.unwrap(),
        })),
        idx,
    );
}

fn parse_function_literal(tokens: &[lexer::Tok]) -> (Result<Box<Node>, error::Err>, usize) {
    let tok = &tokens[0];
    let mut idx = 1;
    let mut arguments: Vec<Box<Node>> = Vec::new();

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    match tok.kind {
        lexer::Token::LeftParen => {
            loop {
                let tk = &tokens[idx];
                match tk.kind {
                    lexer::Token::Identifier => {
                        let id_node = Box::new(IdentifierNode {
                            val: tk.str.clone(),
                            position: tk.position,
                        });
                        arguments.push(id_node)
                    }
                    lexer::Token::EmptyIdentifier => {
                        let id_node = Box::new(EmptyIdentifierNode {
                            position: tk.position,
                        });
                        arguments.push(id_node)
                    }
                    _ => break,
                }
                idx += 1;

                let err = guard_unexpected_input_end(tokens, idx);
                match err {
                    Err(err) => return (Err(err), 0),
                    _ => {}
                }
                match tokens[idx].kind {
                    lexer::Token::Separator => (),
                    _ => {
                        return (
                            Err(error::Err {
                                reason: error::ERR_SYNTAX,
                                message: format!(
                                    "expected arguments in a list separated by {:?}, found {:?}",
                                    lexer::Token::Separator,
                                    tokens[idx]
                                ),
                            }),
                            0,
                        )
                    }
                }
                idx += 1; // GoInk: Separator
            }

            let err = guard_unexpected_input_end(tokens, idx);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
            }

            match tokens[idx].kind {
                lexer::Token::RightParen => {}
                _ => {
                    return (
                        Err(error::Err {
                            reason: error::ERR_SYNTAX,
                            message: format!(
                                "expected arguments list to terminate with {:?}, found {:?}",
                                lexer::Token::RightParen,
                                tokens[idx]
                            ),
                        }),
                        0,
                    )
                }
            }
            idx += 1 // GoInk: RightParen
        }
        lexer::Token::Identifier => {
            let id_node = Box::new(IdentifierNode {
                val: tok.str.clone(),
                position: tok.position,
            });
            arguments.push(id_node)
        }
        lexer::Token::EmptyIdentifier => {
            let id_node = Box::new(EmptyIdentifierNode {
                position: tok.position,
            });
            arguments.push(id_node)
        }
        _ => {
            return (
                Err(error::Err {
                    reason: error::ERR_SYNTAX,
                    message: format!("malformed arguments list in function at {:?}", tok),
                }),
                0,
            )
        }
    }

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    match tokens[idx].kind {
        lexer::Token::FunctionArrow => {
            return (
                Err(error::Err {
                    reason: error::ERR_SYNTAX,
                    message: format!(
                        "expected {:?} but found {:?}",
                        lexer::Token::FunctionArrow,
                        tokens[idx]
                    ),
                }),
                0,
            )
        }
        _ => {}
    }
    idx += 1; // GoInk: FunctionArrow

    let (body, incr) = parse_expression(tokens);
    match body {
        Err(body) => return (Err(body), 0),
        _ => {}
    }
    idx += 1;

    return (
        Ok(Box::new(FunctionLiteralNode {
            arguments: arguments,
            body: body.unwrap(),
            position: tokens[0].position,
        })),
        idx,
    );
}

fn parse_function_call(
    function: Box<Node>,
    tokens: &[lexer::Tok],
) -> (Result<Box<FunctionCallNode>, error::Err>, usize) {
    let mut idx = 1;
    let mut arguments: Vec<Box<Node>> = Vec::new();

    let err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    while tokens[idx].kind != lexer::Token::RightParen {
        let (expr, incr) = parse_expression(&tokens[idx..]);
        match expr {
            Err(expr) => return (Err(expr), 0),
            _ => {}
        }

        idx += 1;
        arguments.push(expr.unwrap());

        let err = guard_unexpected_input_end(tokens, idx);
        match err {
            Err(err) => return (Err(err), 0),
            _ => {}
        }
    }

    idx += 1; // GoInk: RightParen

    return (
        Ok(Box::new(FunctionCallNode {
            function: function,
            arguments: arguments,
        })),
        idx,
    );
}
