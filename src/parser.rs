use crate::error;
use crate::lexer;
use crate::log;
use std::fmt;

#[derive(Debug, Clone)]
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
        clauses: Vec<Node>,
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
        entries: Vec<Node>,
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

trait Position {
    fn pos(&self) -> lexer::Position;
}

impl Position for Node {
    fn pos(&self) -> lexer::Position {
        match self {
            Node::UnaryExprNode { position, .. } => *position,
            Node::MatchClauseNode { target, .. } => target.pos(),
            Node::FunctionCallNode { function, .. } => function.pos(),
            Node::BinaryExprNode { position, .. } => *position,
            Node::MatchExprNode { position, .. } => *position,
            Node::ExpressionListNode { position, .. } => *position,
            Node::EmptyIdentifierNode { position, .. } => *position,
            Node::IdentifierNode { position, .. } => *position,
            Node::NumberLiteralNode { position, .. } => *position,
            Node::StringLiteralNode { position, .. } => *position,
            Node::BooleanLiteralNode { position, .. } => *position,
            Node::ObjectLiteralNode { position, .. } => *position,
            Node::ObjectEntryNode { position, .. } => *position,
            Node::ListLiteralNode { position, .. } => *position,
            Node::FunctionLiteralNode { position, .. } => *position,
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::UnaryExprNode {
                operator, operand, ..
            } => {
                write!(f, "Unary {} ({})", operator, operand)
            }
            Node::MatchClauseNode {
                target, expression, ..
            } => {
                write!(f, "Clause ({}) -> ({})", target, expression)
            }
            Node::FunctionCallNode {
                function,
                arguments,
                ..
            } => {
                write!(
                    f,
                    "Call ({}) on ({})",
                    function,
                    arguments
                        .into_iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            }
            Node::BinaryExprNode {
                left_operand,
                operator,
                right_operand,
                ..
            } => {
                write!(
                    f,
                    "Binary ({}) {:?} ({})",
                    left_operand, operator, right_operand
                )
            }
            Node::MatchExprNode {
                condition, clauses, ..
            } => write!(
                f,
                "Match on ({}) to {{{}}}",
                condition,
                clauses
                    .into_iter()
                    .map(|clause| clause.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::ExpressionListNode { expressions, .. } => {
                write!(
                    f,
                    "Expression List ({})",
                    expressions
                        .into_iter()
                        .map(|expr| expr.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Node::EmptyIdentifierNode { .. } => write!(f, "Empty Identifier"),
            Node::IdentifierNode { val, .. } => write!(f, "Identifier '{}'", val),
            // TODO: add n_to_s call here
            Node::NumberLiteralNode { val, .. } => write!(f, "Number {}", val),
            Node::StringLiteralNode { val, .. } => write!(f, "String {}", val),
            Node::BooleanLiteralNode { val, .. } => write!(f, "Boolean {}", val),
            Node::ObjectLiteralNode { entries, .. } => write!(
                f,
                "Object {}",
                entries
                    .into_iter()
                    .map(|entry| entry.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::ObjectEntryNode { key, val, .. } => {
                write!(f, "Object Entry ({}): ({})", key, val)
            }
            Node::ListLiteralNode { vals, .. } => write!(
                f,
                "List [{}]",
                vals.into_iter()
                    .map(|val| val.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::FunctionLiteralNode {
                body, arguments, ..
            } => {
                write!(
                    f,
                    "Function ({}) => ({})",
                    arguments
                        .into_iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                )
            }
        }
    }
}

// GoInk: Parse transforms a list of Tok (tokens) to Node (AST nodes).
// This implementation uses recursive descent parsing.
pub fn parse(tokens: &Vec<lexer::Tok>, fatal_error: bool, debug_parser: bool) -> Vec<&Node> {
    let mut nodes: Vec<&Node> = Vec::new();
    let mut idx = 0;

    while idx < tokens.len() {
        if tokens[idx].kind == lexer::Token::Separator {
            // GoInk: this sometimes happens when the repl receives comment inputs
            idx += 1;
            continue;
        }

        let (expr, incr) = parse_expression(&tokens[idx..]);
        idx += incr;

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
    left_operand: Node,
    operator: &lexer::Tok,
    tokens: &[lexer::Tok],
    previous_priority: isize,
) -> (Result<Node, error::Err>, usize) {
    let (right_atom, mut idx) = parse_atom(&tokens);
    match right_atom {
        Err(right_atom) => return (Err(right_atom), 0),
        _ => {}
    }
    let incr = 0;

    let mut ops: Vec<&lexer::Tok> = Vec::new();
    let mut nodes: Vec<Node> = Vec::new();
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
            let err = guard_unexpected_input_end(tokens, idx + 1);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
            }

            // GoInk: Priority is higher than previous ops,
            // so make it a right-heavy tree
            let (subtree, incr) = parse_binary_expression(
                nodes.last().unwrap().clone(),
                &tokens[idx],
                &tokens[idx + 1..],
                get_op_priority(ops.last().unwrap()),
            );
            match subtree {
                Err(subtree) => return (Err(subtree), 0),
                _ => {}
            }

            let _last = nodes.len() - 1;
            nodes[_last] = subtree.unwrap();
            idx += incr + 1;
        }
    }

    // GoInk: ops, nodes -> left-biased binary expression tree
    let mut tree = nodes[0].clone();
    nodes.drain(0..1);
    while ops.len() > 0 {
        tree = Node::BinaryExprNode {
            operator: ops[0].kind,
            left_operand: Box::new(tree),
            right_operand: Box::new(nodes[0].clone()),
            position: ops[0].position,
        };
        ops.drain(0..1);
        nodes.drain(0..1);
    }

    return (Ok(tree), idx);
}

fn parse_expression(tokens: &[lexer::Tok]) -> (Result<Node, error::Err>, usize) {
    let mut idx = 0;

    let consume_dangling_separator = |idx: usize, tokens: &[lexer::Tok]| {
        // GoInk: bounds check in case parse_expression called at some point
        // consumed end token
        if idx < tokens.len() && tokens[idx].kind == lexer::Token::Separator {
            idx + 1
        } else {
            idx
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
        // GoInk: these belong to the parent atom that contains this expression,
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

                idx = consume_dangling_separator(idx, tokens);
                return (
                    Ok(Node::MatchExprNode {
                        condition: Box::new(bin_expr.unwrap()),
                        clauses: clauses.unwrap(),
                        position: colon_pos,
                    }),
                    idx,
                );
            }

            idx = consume_dangling_separator(idx, tokens);
            return (Ok(bin_expr.unwrap()), idx);
        }
        lexer::Token::MatchColon => {
            let (clauses, incr) = parse_match_body(&tokens[idx..]);
            match clauses {
                Err(clauses) => return (Err(clauses), 0),
                _ => {}
            }
            idx += incr;

            idx = consume_dangling_separator(idx, tokens);
            return (
                Ok(Node::MatchExprNode {
                    condition: Box::new(atom.unwrap()),
                    clauses: clauses.unwrap(),
                    position: next_tok.position,
                }),
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

fn parse_atom(tokens: &[lexer::Tok]) -> (Result<Node, error::Err>, usize) {
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
            Ok(Node::UnaryExprNode {
                operator: tok.kind,
                operand: Box::new(atom.unwrap()),
                position: tok.position,
            }),
            idx + 1,
        );
    }

    err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    let mut atom: Node;
    match tok.kind {
        lexer::Token::NumberLiteral => {
            return (
                Ok(Node::NumberLiteralNode {
                    val: tok.num,
                    position: tok.position,
                }),
                idx,
            );
        }
        lexer::Token::StringLiteral => {
            return (
                Ok(Node::StringLiteralNode {
                    val: tok.str.clone(),
                    position: tok.position,
                }),
                idx,
            );
        }
        lexer::Token::TrueLiteral => {
            return (
                Ok(Node::BooleanLiteralNode {
                    val: true,
                    position: tok.position,
                }),
                idx,
            );
        }
        lexer::Token::FalseLiteral => {
            return (
                Ok(Node::BooleanLiteralNode {
                    val: false,
                    position: tok.position,
                }),
                idx,
            );
        }
        lexer::Token::Identifier => {
            if tokens[idx].kind == lexer::Token::FunctionArrow {
                let (_atom, _idx) = parse_function_literal(tokens);
                match _atom {
                    Err(_atom) => return (Err(_atom), 0),
                    _ => {
                        atom = _atom.unwrap();
                        idx = _idx
                    }
                }
                // GoInk: parse_atom should not consume trailing Separators, but
                // parse_function_literal does because it ends with expressions.
                // so we backtrack one token.
                idx -= 1
            } else {
                atom = Node::IdentifierNode {
                    val: tok.str.clone(),
                    position: tok.position,
                }
            }
            // GoInk: may be called as a function, so flows beyond
            // switch block
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
                Ok(Node::EmptyIdentifierNode {
                    position: tok.position,
                }),
                idx,
            );
        }
        // GoInk: may be called as a function, so flows beyond
        // switch block
        lexer::Token::LeftParen => {
            // GoInk: grouped expression or function literal
            let mut exprs: Vec<Node> = Vec::new();
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
                    _ => {
                        atom = _atom.unwrap();
                        idx = _idx
                    }
                }

                // GoInk: parse_atom should not consume trailing Separators, but
                // parse_function_literal does because it ends with expressions.
                // so we backtrack one token.
                idx -= 1;
            } else {
                atom = Node::ExpressionListNode {
                    expressions: exprs,
                    position: tok.position,
                }
            }
            // GoInk: may be called as a function, so flows beyond
            // switch block
        }
        lexer::Token::LeftBrace => {
            let mut entries: Vec<Node> = Vec::new();

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
                        let key_expr = key_expr.unwrap();
                        let position = key_expr.pos();
                        entries.push(Node::ObjectEntryNode {
                            key: Box::new(key_expr),
                            val: Box::new(val_expr),
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
                Ok(Node::ObjectLiteralNode {
                    entries: entries,
                    position: tok.position,
                }),
                idx,
            );
        }
        lexer::Token::LeftBracket => {
            let mut vals: Vec<Node> = Vec::new();

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
                Ok(Node::ListLiteralNode {
                    vals: vals,
                    position: tok.position,
                }),
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

    // GoInk: bounds check here because parse_expression may have
    // consumed all tokens before this
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
fn parse_match_body(tokens: &[lexer::Tok]) -> (Result<Vec<Node>, error::Err>, usize) {
    let mut idx = 1; // GoInk: LeftBrace
    let mut clauses: Vec<Node> = Vec::new();

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

fn parse_match_call(tokens: &[lexer::Tok]) -> (Result<Node, error::Err>, usize) {
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
        Ok(Node::MatchClauseNode {
            target: Box::new(atom.unwrap()),
            expression: Box::new(expr.unwrap()),
        }),
        idx,
    );
}

fn parse_match_clause(tokens: &[lexer::Tok]) -> (Result<Node, error::Err>, usize) {
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
        Ok(Node::MatchClauseNode {
            target: Box::new(atom.unwrap()),
            expression: Box::new(expr.unwrap()),
        }),
        idx,
    );
}

fn parse_function_literal(tokens: &[lexer::Tok]) -> (Result<Node, error::Err>, usize) {
    let tok = &tokens[0];
    let mut idx = 1;
    let mut arguments: Vec<Node> = Vec::new();

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
                        let id_node = Node::IdentifierNode {
                            val: tk.str.clone(),
                            position: tk.position,
                        };
                        arguments.push(id_node)
                    }
                    lexer::Token::EmptyIdentifier => {
                        let id_node = Node::EmptyIdentifierNode {
                            position: tk.position,
                        };
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

                if tokens[idx].kind != lexer::Token::Separator {
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
                    );
                }

                idx += 1; // GoInk: Separator
            }

            let err = guard_unexpected_input_end(tokens, idx);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
            }

            if tokens[idx].kind != lexer::Token::RightParen {
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
                );
            }

            idx += 1 // GoInk: RightParen
        }
        lexer::Token::Identifier => {
            let id_node = Node::IdentifierNode {
                val: tok.str.clone(),
                position: tok.position,
            };
            arguments.push(id_node)
        }
        lexer::Token::EmptyIdentifier => {
            let id_node = Node::EmptyIdentifierNode {
                position: tok.position,
            };
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

    if tokens[idx].kind != lexer::Token::FunctionArrow {
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
        );
    }
    idx += 1; // GoInk: FunctionArrow

    let (body, incr) = parse_expression(&tokens[idx..]);
    match body {
        Err(body) => return (Err(body), 0),
        _ => {}
    }
    idx += incr;

    return (
        Ok(Node::FunctionLiteralNode {
            arguments: arguments,
            body: Box::new(body.unwrap()),
            position: tokens[0].position,
        }),
        idx,
    );
}

fn parse_function_call(function: Node, tokens: &[lexer::Tok]) -> (Result<Node, error::Err>, usize) {
    let mut idx = 1;
    let mut arguments: Vec<Node> = Vec::new();

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

        idx += incr;
        arguments.push(expr.unwrap());

        let err = guard_unexpected_input_end(tokens, idx);
        match err {
            Err(err) => return (Err(err), 0),
            _ => {}
        }
    }

    idx += 1; // GoInk: RightParen

    return (
        Ok(Node::FunctionCallNode {
            function: Box::new(function),
            arguments: arguments,
        }),
        idx,
    );
}

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
