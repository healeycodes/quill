use crate::error;
use crate::lexer;
use crate::log;

struct UnaryExprNode {
    operator: lexer::Kind,
    operand: Box<Node>,
    position: lexer::Position,
}
struct BinaryExprNode {
    operator: lexer::Kind,
    left_operand: Box<Node>,
    right_operand: Box<Node>,
    position: lexer::Position,
}
struct FunctionCallNode {
    function: Box<Node>,
    arguments: Vec<Node>,
}
struct MatchClauseNode {
    target: Box<Node>,
    expression: Box<Node>,
}
struct MatchExprNode {
    condition: Box<Node>,
    clauses: Vec<MatchClauseNode>,
    position: lexer::Position,
}
struct ExpressionListNode {
    expressions: Vec<Node>,
    position: lexer::Position,
}
struct EmptyIdentifierNode {
    position: lexer::Position,
}
struct IdentifierNode {
    val: String,
    position: lexer::Position,
}
struct NumberLiteralNode {
    val: f64,
    position: lexer::Position,
}
struct StringLiteralNode {
    val: String,
    position: lexer::Position,
}
struct BooleanLiteralNode {
    val: bool,
    position: lexer::Position,
}
struct ObjectLiteralNode {
    entries: Vec<ObjectEntryNode>,
    position: lexer::Position,
}
struct ObjectEntryNode {
    key: Box<Node>,
    val: Box<Node>,
    position: lexer::Position,
}
struct ListLiteralNode {
    vals: Vec<Node>,
    position: lexer::Position,
}
struct FunctionLiteralNode {
    arguments: Vec<Node>,
    body: Box<Node>,
    position: lexer::Position,
}

#[derive(Debug)]
trait Node {
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

// GoInk(edited): Parse transforms a list of Tok (tokens) to Node (AST nodes).
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
            log::log_debug(&[format!("parse -> {:?}", expr.unwrap())])
        }
    }

    return nodes;
}

fn get_op_priority(t: lexer::Tok) -> isize {
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

fn is_binary_op(t: lexer::Tok) -> bool {
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
    operator: lexer::Tok,
    tokens: &Vec<lexer::Tok>,
) -> Result<(Box<Node>, usize), error::Err> {
    return Ok((
        Box::new(EmptyIdentifierNode {
            position: lexer::Position { line: 1, col: 1 },
        }),
        0,
    ));
    // let (right_atom, idx) = parse_atom(&tokens);
}

fn parse_expression(tokens: &[lexer::Tok]) -> (Result<&Node, error::Err>, usize) {
    let null_node = UnaryExprNode {
        operator: lexer::Kind::AccessorOp,
        operand: Box::new(&EmptyIdentifierNode {
            position: lexer::Position { line: 1, col: 1 },
        }),
        position: lexer::Position { line: 1, col: 1 },
    };
    return (Ok(&null_node), 0);
    // return (
    //     null_node,
    //     0,
    //     Err(error::Err {
    //         message: String::new(),
    //         reason: -1,
    //     }),
    // );
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
    if tok.kind == lexer::Token::NumberLiteral {
        return (
            Ok(Box::new(NumberLiteralNode {
                val: tok.num,
                position: tok.position,
            })),
            idx,
        );
    } else if tok.kind == lexer::Token::StringLiteral {
        return (
            Ok(Box::new(StringLiteralNode {
                val: tok.str.clone(),
                position: tok.position,
            })),
            idx,
        );
    } else if tok.kind == lexer::Token::TrueLiteral {
        return (
            Ok(Box::new(BooleanLiteralNode {
                val: true,
                position: tok.position,
            })),
            idx,
        );
    } else if tok.kind == lexer::Token::FalseLiteral {
        return (
            Ok(Box::new(BooleanLiteralNode {
                val: false,
                position: tok.position,
            })),
            idx,
        );
    } else if tok.kind == lexer::Token::Identifier {
        match tokens[idx].kind {
            lexer::Token::FunctionArrow => {
                let (atom, mut idx) = parse_function_literal(tokens);
                match atom {
                    Err(atom) => return (Err(atom), 0),
                    _ => {}
                }
                idx -= 1
            }
            _ => {
                atom = Box::new(IdentifierNode {
                    val: tok.str.clone(),
                    position: tok.position,
                })
            } // GoInk: may be called as a function, so flows beyond switch block
        }
    } else if tok.kind == lexer::Token::LeftBrace {
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
                    // GoInk (edited): Separator consumed by parse_expression
                    idx += val_incr;
                    entries.push(ObjectEntryNode {
                        key: Box::new(key_expr.unwrap()),
                        val: val_expr,
                        position: (*key_expr.unwrap()).position(),
                    });
                }
            }

            err = guard_unexpected_input_end(tokens, idx);
            match err {
                Err(err) => return (Err(err), 0),
                _ => {}
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
    } else if tok.kind == lexer::Token::LeftBracket {
        let mut vals: Vec<&Node> = Vec::new();

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

    // Del
    let (atom, idx) = parse_atom(&tokens[idx..]);
    return (
        Ok(Box::new(UnaryExprNode {
            operator: tok.kind,
            operand: atom.unwrap(),
            position: tok.position,
        })),
        idx + 1,
    );
}

fn parse_function_literal(tokens: &[lexer::Tok]) -> (Result<&Node, error::Err>, usize) {
    let tok = &tokens[0];
    let mut idx = 1;
    let mut arguments: Vec<&Node> = Vec::new();

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
                        let id_node = &IdentifierNode {
                            val: tk.str.clone(),
                            position: tk.position,
                        };
                        arguments.push(id_node)
                    }
                    lexer::Token::EmptyIdentifier => {
                        let id_node = &EmptyIdentifierNode {
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
            let id_node = &IdentifierNode {
                val: tok.str.clone(),
                position: tok.position,
            };
            arguments.push(id_node)
        }
        lexer::Token::EmptyIdentifier => {
            let id_node = &EmptyIdentifierNode {
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
        Ok(&FunctionLiteralNode {
            arguments: arguments,
            body: Box::new(body),
            position: tokens[0].position,
        }),
        idx,
    );
}
