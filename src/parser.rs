use crate::error;
use crate::lexer;
use crate::log;

#[derive(Debug)]
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
        let (expr, incr, err) = parsed_expression;
        idx += 1;

        match err {
            Err(e) => match e.reason {
                error::ERR_UNKNOWN => log::log_err_f(
                    error::ERR_ASSERT,
                    &[format!("err raised that was not of Err type -> {:?}", e)],
                ),
                _ => {
                    if fatal_error {
                        log::log_err(e.reason, &[e.message])
                    } else {
                        log::log_safe_err(e.reason, &[e.message])
                    }
                }
            },
            _ => {}
        }

        if debug_parser {
            log::log_debug(&[format!("parse -> {:?}", *expr as Node)])
        }
    }

    return nodes;
}

fn get_op_priority(t: lexer::Tok) -> i32 {
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
) -> Result<(Box<Node>, i32), error::Err> {
    return Ok((
        Box::new(Node::EmptyIdentifierNode {
            position: lexer::Position { line: 1, col: 1 },
        }),
        0,
    ));
    // let (right_atom, idx) = parse_atom(&tokens);
}

fn parse_expression(tokens: &[lexer::Tok]) -> (Box<Node>, i32, Result<(), error::Err>) {
    let null_node = Box::new(Node::UnaryExprNode {
        operator: lexer::Kind::AccessorOp,
        operand: Box::new(Node::EmptyIdentifierNode {
            position: lexer::Position { line: 1, col: 1 },
        }),
        position: lexer::Position { line: 1, col: 1 },
    });
    return (null_node, 0, Ok(()));
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
    let idx = 1;

    if tok.kind == lexer::Token::NegationOp {
        let (atom, idx) = parse_atom(&tokens[idx..]);
        match atom {
            Err(atom) => return (Err(atom), 0),
            _ => {}
        }
        return (
            Ok(Box::new(Node::UnaryExprNode {
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
        lexer::Token::NumberLiteral => (
            Box::new(Node::NumberLiteralNode {
                val: tok.num,
                position: tok.position,
            }),
            idx,
        ),
        lexer::Token::StringLiteral => (
            Box::new(Node::StringLiteralNode {
                val: tok.str.clone(),
                position: tok.position,
            }),
            idx,
        ),
        lexer::Token::TrueLiteral => (
            Box::new(Node::BooleanLiteralNode {
                val: true,
                position: tok.position,
            }),
            idx,
        ),
        lexer::Token::FalseLiteral => (
            Box::new(Node::BooleanLiteralNode {
                val: false,
                position: tok.position,
            }),
            idx,
        ),
        lexer::Token::Identifier => {
            match tokens[idx].kind {
                lexer::Token::FunctionArrow => {
                    let (atom, idx) = parse_function_literal(tokens);

                    // if err != nil {
                    //     return nil, 0, err
                    // }
                    // // parseAtom should not consume trailing Separators, but
                    // // 	parseFunctionLiteral does because it ends with expressions.
                    // // 	so we backtrack one token.
                    // idx--
                }
                _ => {
                    atom = Box::new(Node::IdentifierNode {
                        val: tok.str.clone(),
                        position: tok.position,
                    })
                }
            }
        }
    };

    // Del
    let (atom, idx) = parse_atom(&tokens[idx..]);
    return (
        Ok(Box::new(Node::UnaryExprNode {
            operator: tok.kind,
            operand: atom.unwrap(),
            position: tok.position,
        })),
        idx + 1,
    );
}

fn parse_function_literal(tokens: &[lexer::Tok]) -> (Result<Box<Node>, error::Err>, usize) {
    let tok = tokens[0];
    let mut idx = 1;
    let arguments: Vec<Box<Node>> = Vec::new();

    err = guard_unexpected_input_end(tokens, idx);
    match err {
        Err(err) => return (Err(err), 0),
        _ => {}
    }

    match tok.kind {
        lexer::Token::LeftParen => {
            while true {
                let tk = tokens[idx];
                match tk.kind {
                    lexer::Token::Identifier => {
                        let id_node = Box::new(Node::IdentifierNode {
                            val: tk.str,
                            position: tk.position,
                        });
                        arguments.push(id_node)
                    }
                    lexer::Token::EmptyIdentifier => {
                        let id_node = Box::new(Node::EmptyIdentifierNode {
                            position: tk.position,
                        });
                        arguments.push(id_node)
                    }
                    _ => break,
                }
                idx += 1;

                err = guard_unexpected_input_end(tokens, idx);
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
                                    "expected arguments in a list separated by {}, found {}}",
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

            err = guard_unexpected_input_end(tokens, idx);
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
                                "expected arguments list to terminate with {}, found {}",
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
            let id_node = Box::new(Node::IdentifierNode {
                val: tk.str,
                position: tk.position,
            });
            arguments.push(id_node)
        }
        lexer::Token::EmptyIdentifier => {
            let id_node = Box::new(Node::EmptyIdentifier {
                position: tk.position,
            });
            arguments.push(id_node)
        }
        _ => {
            return (
                Err(error::Err {
                    reason: error::ERR_SYNTAX,
                    message: format!("malformed arguments list in function at {}", tok),
                }),
                0,
            )
        }
    }
}
