use crate::error;
use crate::lexer;
use crate::log;
use crate::parser;
use std::{
    cmp,
    collections::HashMap,
    fmt, fs,
    path::Path,
    str,
    sync::{
        atomic::{AtomicI32, Ordering::SeqCst},
        Arc, Mutex, RwLock,
    },
};

use unicode_segmentation::UnicodeSegmentation;

const MAX_PRINT_LEN: usize = 120;

// GoInk: Value represents any value in the Ink programming language.
// Each value corresponds to some primitive or object value created
// during the execution of an Ink program.
#[derive(Clone)]
pub enum Value {
    // GoInk: EmptyValue is the value of the empty identifier.
    // it is globally unique and matches everything in equality.
    EmptyValue {},
    // GoInk: NumberValue represents the number type (integer and floating point)
    // in the Ink language.
    NumberValue(f64),
    // GoInk: StringValue represents all characters and strings in Ink
    StringValue(Vec<u8>),
    // GoInk: BooleanValue is either `true` or `false`
    BooleanValue(bool),
    // GoInk: NullValue is a value that only exists at the type level,
    // and is represented by the empty expression list `()`.
    NullValue(u8),
    // GoInk: CompositeValue includes all objects and list values
    CompositeValue(ValueTable),
    // GoInk: FunctionValue is the value of any variables referencing functions
    // defined in an Ink program.
    FunctionValue(FunctionValue),
    // GoInk: FunctionCallThunkValue is an internal representation of a lazy
    // function evaluation used to implement tail call optimization.
    FunctionCallThunkValue(FunctionCallThunkValue),
    // GoInk: NativeFunctionValue represents a function whose implementation is written
    // in Rust and built-into the runtime.
    NativeFunctionValue(NativeFunctionValue),
}

#[derive(Clone)]
pub struct FunctionValue {
    defn: parser::Node, // FunctionLiteralNode
    parent_frame: Arc<RwLock<StackFrame>>,
}

#[derive(Clone)]
pub struct FunctionCallThunkValue {
    vt: ValueTable,
    function: FunctionValue,
}

#[derive(Clone)]
pub struct NativeFunctionValue {
    pub name: String,
}

// GoInk: The singleton Null value is interned into a single value
pub const NULL: Value = Value::NullValue(0);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::EmptyValue {} => write!(f, "_"),
            Value::NumberValue(n) => write!(f, "{}", nv_to_s(*n)),
            Value::StringValue(s) => {
                write!(
                    f,
                    "'{}'",
                    String::from_utf8(s.clone())
                        .unwrap()
                        .replace("\\", "\\\\")
                        .replace("'", "\\'")
                )
            }
            Value::BooleanValue(b) => write!(f, "{}", b),
            Value::NullValue(_) => write!(f, "()"),
            Value::CompositeValue(vt) => write!(f, "{}", value_table_to_string(vt)),
            Value::FunctionValue(fv) => {
                write!(f, "{}", function_value_to_string(&fv))
            }
            Value::FunctionCallThunkValue(ft) => {
                write!(f, "Thunk of ({})", function_value_to_string(&ft.function))
            }
            Value::NativeFunctionValue(nf) => write!(f, "Native Function ({})", nf.name),
        }
    }
}

pub fn value_table_to_string(vt: &ValueTable) -> String {
    let mut entries: Vec<String> = Vec::new();
    for (key, value) in vt {
        entries.push(format!("{}: {}", key, value))
    }
    return format!("{{{}}}", entries.join(", "));
}

fn function_value_to_string(fv: &FunctionValue) -> String {
    // GoInk: ellipsize function body at a reasonable length,
    // so as not to be too verbose in repl environments
    let mut fstr = fv.defn.to_string();
    if fstr.len() > MAX_PRINT_LEN {
        fstr = [fstr[MAX_PRINT_LEN..].to_string(), "..".to_string()].join("")
    }
    fstr
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if matches!(other, Value::EmptyValue {}) {
            true
        } else {
            match &*self {
                Value::EmptyValue {} => true,
                Value::NumberValue(n) => {
                    if let Value::NumberValue(o) = other {
                        *n as f64 == *o as f64
                    } else {
                        false
                    }
                }
                Value::StringValue(s) => {
                    if let Value::StringValue(o) = other {
                        s == o
                    } else {
                        false
                    }
                }
                Value::BooleanValue(b) => {
                    if let Value::BooleanValue(o) = other {
                        *b as bool == *o as bool
                    } else {
                        false
                    }
                }
                Value::NullValue(_b) => {
                    if let Value::NullValue(_o) = other {
                        true
                    } else {
                        false
                    }
                }
                Value::CompositeValue(vt) => {
                    if let Value::CompositeValue(o) = other {
                        let _vt = &*vt as &ValueTable;
                        let _o = &*o as &ValueTable;
                        if _vt.keys().len() != _o.keys().len() {
                            return false;
                        }
                        for (k, v) in _vt.iter() {
                            match _o.get(k) {
                                Some(_o_value) => {
                                    if v != _o_value {
                                        return false;
                                    }
                                }
                                None => return false,
                            }
                        }
                        true
                    } else {
                        false
                    }
                }
                Value::FunctionValue(f) => {
                    // GoInk: to compare structs containing slices, we really want
                    // a pointer comparison, not a value comparison
                    if let Value::FunctionValue(o) = other {
                        // Position _should_ be unique
                        f.defn.pos() == o.defn.pos()
                    } else {
                        false
                    }
                }
                Value::FunctionCallThunkValue(ft) => {
                    // GoInk: to compare structs containing slices, we really want
                    // a pointer comparison, not a value comparison
                    if let Value::FunctionCallThunkValue(o) = other {
                        // Position _should_ be unique
                        ft.vt == o.vt && ft.function.defn.pos() == o.function.defn.pos()
                    } else {
                        false
                    }
                }
                Value::NativeFunctionValue(nf) => {
                    if let Value::NativeFunctionValue(o) = other {
                        nf.name == o.name
                    } else {
                        false
                    }
                }
            }
        }
    }
}

fn is_intable(v: Value) -> bool {
    // GoInk: Note: this returns false for int64 outside of the float64 range,
    // but that's ok since is_intable is used to check before ops that will
    // convert values to float64's (NumberValues) anyway
    if let Value::NumberValue(n) = v {
        return n == (n as i64) as f64;
    }
    // TODO: maybe just return false here?
    panic!("is_intable was called with incompatible value {}", v);
}

// GoInk: Utility func to get a consistent, language spec-compliant
// string representation of numbers
pub fn n_to_s(f: f64) -> String {
    // GoInk: Prefer exact integer form if possible
    if f - f.round() == 0.0 {
        // TODO: strconv.FormatFloat(f, 'g', -1, 64)
        return format!("{}", f);
    }

    return format!("{}", f);
}

// GoInk: n_to_s for NumberValue type
pub fn nv_to_s(v: f64) -> String {
    return n_to_s(v);
}

// zero-extend a vector of u8 bytes to given length
fn zero_extend(s: Vec<u8>, max: usize) -> Vec<u8> {
    if max <= s.len() {
        return s;
    }

    let mut extended = s.clone();
    for _ in 0..max - extended.len() {
        extended.push(0)
    }

    return extended;
}

// GoInk: return the max length of two slices
fn max_len(a: &Vec<u8>, b: &Vec<u8>) -> usize {
    return cmp::max(a.len(), b.len());
}

// GoInk: ValueTable is used anytime a map of names/labels to Ink Values is needed,
// and is notably used to represent stack frames / heaps and CompositeValue dictionaries.
type ValueTable = HashMap<String, Value>;

// GoInk: StackFrame represents the heap of variables local to a particular function call frame,
// and recursively references other parent StackFrames internally.
#[derive(Clone)]
pub struct StackFrame {
    pub parent: Option<Arc<RwLock<StackFrame>>>,
    pub vt: ValueTable,
}

impl StackFrame {
    // GoInk: Get a value from the stack frame chain
    fn get(&self, name: &String) -> Option<Value> {
        if let Some(value) = self.vt.get(name) {
            return Some(value.clone());
        }
        if let None = self.parent {
            return None;
        }
        return self.parent.as_ref().unwrap().read().unwrap().get(name);
    }

    // GoInk: Set a value to the most recent call stack frame
    pub fn set(&mut self, name: String, val: Value) {
        self.vt.insert(name, val);
    }
    // GoInk: Up updates a value in the stack frame chain
    fn up(&mut self, name: String, val: Value) {
        if self.vt.contains_key(&name) {
            self.vt.insert(name, val);
            return;
        }
        if let Some(parent) = &mut self.parent {
            parent.write().unwrap().up(name, val)
        }
    }
}

impl Engine {
    // GoInk: create_contex creates and initializes a new Context tied to a given Engine.
    pub fn create_context(&self, engine: &Arc<RwLock<Engine>>) -> Context {
        let ctx = Context {
            cwd: String::new(),
            file: String::new(),
            engine: Arc::clone(&*engine),
            frame: Arc::new(RwLock::new(StackFrame {
                parent: Option::None,
                vt: ValueTable::new(),
            })),
            event_channel: tokio::sync::mpsc::unbounded_channel::<Message>(),
        };
        ctx.load_environment();

        // TODO: ctx.reset_wd()
        ctx
    }
}

// A message is the result of async work, passed through a Tokio channel
pub enum Message {
    // Error(Err),
    Result(Result<Value, error::Err>),
    InkFunctionCallback((Value, bool, Vec<Value>)),
}

// GoInk: eval takes Nodes to evaluate, and executes the Ink programs defined
// in the syntax tree. eval returns the last value of the last expression in the AST,
// or an error if there was a runtime error.
impl Context {
    fn eval(&mut self, nodes: Vec<parser::Node>, dump_frame: bool) -> Value {
        // TODO: eval lock here?
        let mut result = Value::EmptyValue {};
        for node in nodes.iter() {
            let val = self.node_eval(&*node, self.frame.clone(), false);
            if let Err(err) = val {
                self.log_err(err);
                break;
            } else {
                result = val.unwrap()
            }
        }
        if dump_frame {
            self.dump()
        }
        result
    }

    fn node_eval(
        &self,
        node: &parser::Node,
        frame: Arc<RwLock<StackFrame>>,
        allow_thunk: bool,
    ) -> Result<Value, error::Err> {
        if let parser::Node::FunctionLiteralNode { .. } = node {
            return Ok(Value::FunctionValue(FunctionValue {
                defn: node.clone(),
                parent_frame: frame,
            }));
        }
        match node {
            parser::Node::UnaryExprNode {
                operator,
                operand,
                position,
                ..
            } => self.eval_unary(frame, allow_thunk, *operator, &operand, *position),
            parser::Node::BinaryExprNode {
                operator,
                left_operand,
                right_operand,
                position,
                ..
            } => self.eval_binary(
                frame,
                allow_thunk,
                *operator,
                &left_operand,
                &right_operand,
                *position,
            ),
            parser::Node::FunctionCallNode {
                function,
                arguments,
                ..
            } => self.eval_function_call(frame, allow_thunk, &function, arguments.clone()),
            parser::Node::MatchClauseNode { .. } => {
                let assert_err = error::Err {
                    reason: error::ERR_ASSERT,
                    message: "cannot Eval a MatchClauseNode".to_string(),
                };
                log::log_err_f(assert_err.reason, &[assert_err.message.to_string()]);
                Err(assert_err)
            }
            parser::Node::MatchExprNode {
                condition, clauses, ..
            } => self.eval_match_expr(frame, allow_thunk, &condition, clauses.clone()),
            parser::Node::ExpressionListNode { expressions, .. } => {
                self.eval_expression_list(frame, allow_thunk, expressions.clone())
            }
            parser::Node::EmptyIdentifierNode { .. } => Ok(Value::EmptyValue {}),
            parser::Node::IdentifierNode { val, position, .. } => {
                self.eval_identifier(frame, allow_thunk, val.to_string(), *position)
            }
            parser::Node::NumberLiteralNode { val, .. } => Ok(Value::NumberValue(*val)),
            parser::Node::StringLiteralNode { val, .. } => Ok(Value::StringValue((*val).clone())),
            parser::Node::BooleanLiteralNode { val, .. } => Ok(Value::BooleanValue(*val)),
            parser::Node::ObjectLiteralNode { entries, .. } => {
                self.eval_object_literal(frame, allow_thunk, entries.clone())
            }
            parser::Node::ObjectEntryNode { .. } => {
                let assert_err = error::Err {
                    reason: error::ERR_ASSERT,
                    message: "cannot Eval a ObjectEntryNode".to_string(),
                };
                log::log_err_f(assert_err.reason, &[assert_err.message.to_string()]);
                Err(assert_err)
            }
            parser::Node::ListLiteralNode { vals, .. } => {
                self.eval_list_literal(frame, allow_thunk, vals.clone())
            }
            _ => Ok(Value::EmptyValue {}),
        }
    }

    fn operand_to_string(
        &self,
        right_operand: parser::Node,
        frame: Arc<RwLock<StackFrame>>,
    ) -> Result<String, error::Err> {
        match right_operand {
            parser::Node::IdentifierNode { val, .. } => return Ok(val),
            parser::Node::StringLiteralNode { val, .. } => {
                return Ok(String::from_utf8(val).unwrap())
            }
            parser::Node::NumberLiteralNode { val, .. } => return Ok(n_to_s(val)),
            _ => {
                let right_evaluated_value =
                    self.node_eval(&right_operand, Arc::clone(&frame), false)?;
                match right_evaluated_value {
                    Value::StringValue(s) => return Ok(String::from_utf8(s).unwrap()),
                    Value::NumberValue(n) => return Ok(nv_to_s(n)),
                    _ => {
                        return Err(error::Err {
                            reason: error::ERR_RUNTIME,
                            message: format!(
                                "cannot access invalid property name {} of a composite value [{}]",
                                right_evaluated_value,
                                right_operand.pos()
                            ),
                        })
                    }
                }
            }
        }
    }
    // GoInk: unwrap_thunk expands out a recursive structure of thunks
    // into a flat for loop control structure
    fn unwrap_thunk(&self, mut thunk: FunctionCallThunkValue) -> Result<Value, error::Err> {
        loop {
            let frame = Arc::new(RwLock::new(StackFrame {
                parent: Some(Arc::clone(&thunk.function.parent_frame)),
                vt: thunk.vt.clone(),
            }));
            if let parser::Node::FunctionLiteralNode { body, .. } = thunk.function.defn {
                let v = self.node_eval(&*body, Arc::clone(&frame), true)?;
                if let Value::FunctionCallThunkValue(fcallthunk) = v {
                    thunk = FunctionCallThunkValue {
                        vt: fcallthunk.vt,
                        function: fcallthunk.function,
                    };
                    continue;
                }
                return Ok(v);
            }
        }
        // TODO: refactor to remove this branch or keep to maintain similarity to GoInk?
        panic!("unreachable block inside unwrap_thunk");
    }

    fn eval_unary(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        _allow_thunk: bool,
        operator: lexer::Kind,
        operand: &Box<parser::Node>,
        position: lexer::Position,
    ) -> Result<Value, error::Err> {
        let operand = self.node_eval(&**operand, Arc::clone(&frame), false);
        if let Err(err) = operand {
            return Err(err);
        } else if !matches!(operator, lexer::Token::NegationOp) {
            let assert_err = error::Err {
                reason: error::ERR_ASSERT,
                message: format!("unrecognized unary operator {}", operator),
            };
            log::log_err_f(assert_err.reason, &[assert_err.message.clone()]);
            return Err(assert_err);
        }
        let _operand = operand.unwrap();
        match _operand {
            Value::NumberValue(n) => return Ok(Value::NumberValue(-n)),
            Value::BooleanValue(b) => return Ok(Value::BooleanValue(!b)),
            _ => {
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "cannot negate non-boolean and non-number value {} [{}]",
                        _operand, position
                    ),
                })
            }
        };
    }

    fn eval_binary(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        _allow_thunk: bool,
        operator: lexer::Kind,
        left_operand: &Box<parser::Node>,
        right_operand: &Box<parser::Node>,
        _position: lexer::Position,
    ) -> Result<Value, error::Err> {
        if matches!(operator, lexer::Token::DefineOp) {
            if let parser::Node::IdentifierNode { val, position, .. } = &**left_operand {
                if let parser::Node::EmptyIdentifierNode { .. } = **right_operand {
                    return Err(error::Err {
                        reason: error::ERR_RUNTIME,
                        message: format!(
                            "cannot assign an empty identifier value to {} [{}]",
                            val, position
                        ),
                    });
                } else {
                    let right_value =
                        self.node_eval(&**right_operand, Arc::clone(&frame), false)?;
                    frame.write().unwrap().set(val.clone(), right_value.clone());
                    return Ok(right_value);
                }
            }
            if let parser::Node::BinaryExprNode {
                operator,
                left_operand,
                right_operand,
                position,
                ..
            } = &**left_operand
            {
                if *operator == lexer::Token::AccessorOp {
                    let mut left_value =
                        self.node_eval(&**left_operand, Arc::clone(&frame), false)?;
                    let left_key =
                        self.operand_to_string((**right_operand).clone(), Arc::clone(&frame))?;
                    if let Value::CompositeValue(mut vt) = left_value {
                        let right_value = self.node_eval(&**right_operand, frame, false)?;
                        vt.insert(left_key, right_value);
                        return Ok(Value::CompositeValue(vt));
                    } else if let Value::StringValue(ref mut left_string) = left_value {
                        if let parser::Node::IdentifierNode { val, .. } = &**left_operand {
                            let right_value =
                                self.node_eval(&**right_operand, Arc::clone(&frame), false)?;
                            if let Value::StringValue(mut right_string) = right_value {
                                let right_num = left_key.parse::<i64>();
                                if let Err(_right_num) = right_num {
                                    return Err(error::Err{
										reason: error::ERR_RUNTIME,
										message: format!("while accessing string {} at an index, found non-integer index {} [{}]",
										Value::StringValue(left_string.clone()), left_key, right_operand.pos()
									)
									});
                                }
                                let rn = right_num.unwrap() as usize;
                                let mut new_left_string = left_string.clone();
                                if 0 == rn && rn < left_string.len() {
                                    for (i, r) in left_string.iter().enumerate() {
                                        if rn + i < left_string.len() {
                                            new_left_string[rn + i] = *r
                                        } else {
                                            new_left_string.push(*r)
                                        }
                                    }
                                    frame.write().unwrap().up(
                                        val.clone(),
                                        Value::StringValue(new_left_string.clone()),
                                    );
                                    return Ok(Value::StringValue(new_left_string));
                                } else if rn == left_string.len() {
                                    left_string.append(&mut right_string);
                                    frame
                                        .write()
                                        .unwrap()
                                        .up(val.clone(), Value::StringValue(left_string.clone()));
                                    return Ok(Value::StringValue(left_string.clone()));
                                } else {
                                    return Err(error::Err {
                                        reason: error::ERR_RUNTIME,
                                        message: format!(
											"tried to modify string {} at out of bounds index {} [{}]",
											Value::StringValue(left_string.clone()),
											left_key,
											right_operand.pos()
										),
                                    });
                                }
                            } else {
                                return Err(error::Err {
                                    reason: error::ERR_RUNTIME,
                                    message: format!(
                                        "cannot set part of string to a non-character {}",
                                        right_value
                                    ),
                                });
                            }
                        } else {
                            return Err(error::Err {
                                reason: error::ERR_RUNTIME,
                                message: format!(
									"cannot set string {} at index because string is not an identifier",
									left_operand
								),
                            });
                        }
                    } else {
                        return Err(error::Err {
                            reason: error::ERR_RUNTIME,
                            message: format!(
                                "cannot set property of a non-composite value {} [{}]",
                                left_value,
                                left_operand.pos()
                            ),
                        });
                    }
                }
            }
        } else if matches!(operator, lexer::Token::AccessorOp) {
            let left_value = self.node_eval(&**left_operand, Arc::clone(&frame), false)?;
            let right_value_str =
                self.operand_to_string((**right_operand).clone(), Arc::clone(&frame))?;
            if let Value::CompositeValue(ref left_value_composite) = left_value {
                if !left_value_composite.contains_key(&right_value_str) {
                    return Ok(NULL);
                }
                return Ok((*left_value_composite.get(&right_value_str).unwrap()).clone());
            } else if let Value::StringValue(left_string) = left_value {
                let right_num = right_value_str.parse::<i64>();
                if let Err(_right_num) = right_num {
                    return Err(error::Err {
                        reason: error::ERR_RUNTIME,
                        message: format!(
							"while accessing string {} at an index, found non-integer index {} [{}]",
							Value::StringValue(left_string),
							right_value_str,
							right_operand.pos()
						),
                    });
                }
                let rn = right_num.unwrap() as usize;
                if 0 == rn && rn < left_string.len() {
                    return Ok(Value::StringValue([left_string[rn]].to_vec()));
                }
                return Ok(NULL);
            } else {
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "cannot access property {} of a non-composite value {} [{}]",
                        right_operand,
                        left_value,
                        right_operand.pos()
                    ),
                });
            }
        }
        let left_value = self.node_eval(&**left_operand, Arc::clone(&frame), false)?;
        let right_value = self.node_eval(&**right_operand, Arc::clone(&frame), false)?;
        match operator {
            lexer::Token::AddOp => {
                match left_value {
                    Value::NumberValue(ref left) => {
                        if let Value::NumberValue(right) = right_value {
                            return Ok(Value::NumberValue(*left + right));
                        }
                    }
                    Value::StringValue(ref left) => {
                        if let Value::StringValue(right) = right_value {
                            // GoInk: In this context, strings are immutable. i.e. concatenating
                            // strings should produce a completely new string whose modifications
                            // won't be observable by the original strings.
                            return Ok(Value::StringValue(
                                left.iter().chain(&right).cloned().collect(),
                            ));
                        }
                    }
                    Value::BooleanValue(ref left) => {
                        if let Value::BooleanValue(right) = right_value {
                            return Ok(Value::BooleanValue(*left || right));
                        }
                    }
                    _ => {}
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support addition [{}]",
                        left_value, right_value, operator
                    ),
                });
            }
            lexer::Token::SubtractOp => {
                if let Value::NumberValue(left) = left_value {
                    if let Value::NumberValue(right) = right_value {
                        return Ok(Value::NumberValue(left - right));
                    }
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support subtraction [{}]",
                        left_value, right_value, operator
                    ),
                });
            }
            lexer::Token::MultiplyOp => {
                match left_value {
                    Value::NumberValue(left) => {
                        if let Value::NumberValue(right) = right_value {
                            return Ok(Value::NumberValue(left * right));
                        }
                    }
                    Value::BooleanValue(left) => {
                        if let Value::BooleanValue(right) = right_value {
                            return Ok(Value::BooleanValue(left && right));
                        }
                    }
                    _ => {}
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support multiplication [{}]",
                        left_value, right_value, operator
                    ),
                });
            }
            lexer::Token::DivideOp => {
                if let Value::NumberValue(left) = left_value {
                    if let Value::NumberValue(right) = right_value {
                        if right == 0.0 {
                            return Err(error::Err {
                                reason: error::ERR_RUNTIME,
                                message: format!(
                                    "division by zero error [{}]",
                                    right_operand.pos()
                                ),
                            });
                        }
                        return Ok(Value::NumberValue(left / right));
                    }
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support division [{}]",
                        left_value, right_value, operator
                    ),
                });
            }
            lexer::Token::ModulusOp => {
                if let Value::NumberValue(left) = left_value {
                    if let Value::NumberValue(right) = right_value {
                        if right == 0.0 {
                            return Err(error::Err {
                                reason: error::ERR_RUNTIME,
                                message: format!(
                                    "division by zero error in modulus [{}]",
                                    right_operand.pos()
                                ),
                            });
                        }
                        if is_intable(Value::NumberValue(right)) {
                            return Ok(Value::NumberValue(left % right));
                        }
                        return Err(error::Err {
                            reason: error::ERR_RUNTIME,
                            message: format!(
                                "cannot take modulus of non-integer value {} [{}]",
                                nv_to_s(right),
                                left_operand.pos()
                            ),
                        });
                    }
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support modulus [{}]",
                        left_value,
                        right_value,
                        right_operand.pos()
                    ),
                });
            }
            lexer::Token::LogicalAndOp => {
                match left_value {
                    Value::NumberValue(left) => {
                        if let Value::NumberValue(right) = right_value {
                            if is_intable(Value::NumberValue(left))
                                && is_intable(Value::NumberValue(right))
                            {
                                return Ok(Value::NumberValue((left as i64 & right as i64) as f64));
                            }
                            return Err(error::Err {
                                reason: error::ERR_RUNTIME,
                                message: format!(
                                    "cannot take logical & of non-integer values {}, {} [{}]",
                                    nv_to_s(right),
                                    nv_to_s(left),
                                    right_operand.pos()
                                ),
                            });
                        }
                    }
                    Value::StringValue(ref left) => {
                        if let Value::StringValue(right) = right_value {
                            let max = max_len(left, &right);
                            let a = zero_extend(left.clone(), max);
                            let b = zero_extend(right.clone(), max);
                            let mut c: Vec<u8> = Vec::new();
                            for i in 0..max {
                                c[i] = a[i] & b[i]
                            }
                            return Ok(Value::StringValue(c));
                        }
                    }
                    Value::BooleanValue(ref left) => {
                        if let Value::BooleanValue(right) = right_value {
                            return Ok(Value::BooleanValue(*left && right));
                        }
                    }
                    _ => {}
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support bitwise or logical & [{}]",
                        left_value,
                        right_value,
                        right_operand.pos()
                    ),
                });
            }
            lexer::Token::LogicalOrOp => {
                match left_value {
                    Value::NumberValue(ref left) => {
                        if let Value::NumberValue(right) = right_value {
                            if is_intable(Value::NumberValue(*left))
                                && is_intable(Value::NumberValue(right))
                            {
                                return Ok(Value::NumberValue(
                                    (*left as i64 | right as i64) as f64,
                                ));
                            }
                            return Err(error::Err {
                                reason: error::ERR_RUNTIME,
                                message: format!(
                                    "cannot take logical | of non-integer values {}, {} [{}]",
                                    nv_to_s(right),
                                    nv_to_s(*left),
                                    right_operand.pos()
                                ),
                            });
                        }
                    }
                    Value::StringValue(ref left) => {
                        if let Value::StringValue(right) = right_value {
                            let max = max_len(left, &right);
                            let a = zero_extend(left.clone(), max);
                            let b = zero_extend(right.clone(), max);
                            let mut c: Vec<u8> = Vec::new();
                            for i in 0..max {
                                c[i] = a[i] | b[i]
                            }
                            return Ok(Value::StringValue(c));
                        }
                    }
                    Value::BooleanValue(ref left) => {
                        if let Value::BooleanValue(right) = right_value {
                            return Ok(Value::BooleanValue(*left || right));
                        }
                    }
                    _ => {}
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support bitwise or logical | [{}]",
                        left_value,
                        right_value,
                        right_operand.pos()
                    ),
                });
            }
            lexer::Token::LogicalXorOp => {
                match left_value {
                    Value::NumberValue(ref left) => {
                        if let Value::NumberValue(right) = right_value {
                            if is_intable(Value::NumberValue(*left))
                                && is_intable(Value::NumberValue(right))
                            {
                                return Ok(Value::NumberValue(
                                    (*left as i64 ^ right as i64) as f64,
                                ));
                            }
                            return Err(error::Err {
                                reason: error::ERR_RUNTIME,
                                message: format!(
                                    "cannot take logical ^ of non-integer values {}, {} [{}]",
                                    nv_to_s(right),
                                    nv_to_s(*left),
                                    right_operand.pos()
                                ),
                            });
                        }
                    }
                    Value::StringValue(ref left) => {
                        if let Value::StringValue(right) = right_value {
                            let max = max_len(left, &right);
                            let a = zero_extend(left.clone(), max);
                            let b = zero_extend(right.clone(), max);
                            let mut c: Vec<u8> = Vec::new();
                            for i in 0..max {
                                c[i] = a[i] & b[i]
                            }
                            return Ok(Value::StringValue(c));
                        }
                    }
                    Value::BooleanValue(ref left) => {
                        if let Value::BooleanValue(right) = right_value {
                            return Ok(Value::BooleanValue(*left && right));
                        }
                    }
                    _ => {}
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support bitwise or logical ^ [{}]",
                        left_value,
                        right_value,
                        right_operand.pos()
                    ),
                });
            }
            lexer::Token::GreaterThanOp => {
                match left_value {
                    Value::NumberValue(ref left) => {
                        if let Value::NumberValue(right) = right_value {
                            return Ok(Value::BooleanValue(left > &right));
                        }
                    }
                    Value::StringValue(ref left) => {
                        if let Value::StringValue(right) = right_value {
                            return Ok(Value::BooleanValue(left > &right));
                        }
                    }
                    _ => {}
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support comparison [{}]",
                        left_value,
                        right_value,
                        right_operand.pos()
                    ),
                });
            }
            lexer::Token::LessThanOp => {
                if let Value::NumberValue(ref left) = left_value {
                    if let Value::NumberValue(right) = right_value {
                        return Ok(Value::BooleanValue(*left < right));
                    }
                } else if let Value::StringValue(ref left) = left_value {
                    if let Value::StringValue(right) = right_value {
                        return Ok(Value::BooleanValue(*left < right));
                    }
                }
                return Err(error::Err {
                    reason: error::ERR_RUNTIME,
                    message: format!(
                        "values {} and {} do not support comparison [{}]",
                        left_value,
                        right_value,
                        right_operand.pos()
                    ),
                });
            }
            lexer::Token::EqualOp => return Ok(Value::BooleanValue(left_value == right_value)),
            _ => {}
        }
        let assert_err = error::Err {
            reason: error::ERR_ASSERT,
            message: format!("unknown binary operator {}", operator),
        };
        log::log_err_f(assert_err.reason, &[assert_err.message.to_string()]);
        return Err(assert_err);
    }

    fn eval_identifier(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        _allow_thunk: bool,
        val: String,
        position: lexer::Position,
    ) -> Result<Value, error::Err> {
        if let Some(value) = frame.read().unwrap().get(&val) {
            return Ok(value);
        }
        Err(error::Err {
            reason: error::ERR_RUNTIME,
            message: format!("{} is not defined [{}]", val, position),
        })
    }

    fn eval_function_call(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        allow_thunk: bool,
        function: &Box<parser::Node>,
        arguments: Vec<parser::Node>,
    ) -> Result<Value, error::Err> {
        let fun = self.node_eval(&**function, Arc::clone(&frame), false)?;
        let mut arg_results: Vec<Value> = Vec::new();
        for arg in arguments {
            arg_results.push(self.node_eval(&arg, Arc::clone(&frame), false)?)
        }
        return self.eval_ink_function(fun, allow_thunk, arg_results.clone());
    }
    // GoInk: call into an Ink callback function synchronously
    pub fn eval_ink_function(
        &self,
        fun: Value,
        allow_thunk: bool,
        args: Vec<Value>,
    ) -> Result<Value, error::Err> {
        if let Value::FunctionValue(ref funv) = fun {
            let mut arg_value_table = ValueTable::new();
            if let parser::Node::FunctionLiteralNode { arguments, .. } = &funv.defn {
                for (i, arg_node) in arguments.iter().enumerate() {
                    if i < args.len() {
                        if let parser::Node::IdentifierNode { val, .. } = arg_node {
                            arg_value_table.insert(val.to_string(), args[i].clone());
                        }
                    }
                }
                // GoInk: Tail Call Optimization used for evaluating expressions that may be in tail positions
                // at the end of Nodes whose evaluation allocates another StackFrame
                // like ExpressionListNode and FunctionLiteralNode's body
                let return_thunk = FunctionCallThunkValue {
                    vt: arg_value_table,
                    function: funv.clone(),
                };
                if allow_thunk {
                    return Ok(Value::FunctionCallThunkValue(return_thunk));
                }
                return self.unwrap_thunk(return_thunk);
            }
        }
        if let Value::NativeFunctionValue(nfun) = fun {
            return self.native_function(nfun.name, args);
        }
        return Err(error::Err {
            reason: error::ERR_RUNTIME,
            message: format!("attempted to call a non-function value {}", fun),
        });
    }

    fn eval_match_expr(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        allow_thunk: bool,
        condition: &Box<parser::Node>,
        clauses: Vec<parser::Node>,
    ) -> Result<Value, error::Err> {
        let condition_val = self.node_eval(&**condition, Arc::clone(&frame), false)?;
        for clause in clauses {
            if let parser::Node::MatchClauseNode { target, expression } = clause {
                let target_val = self.node_eval(&*target, Arc::clone(&frame), false)?;
                if condition_val == target_val {
                    // GoInk: match expression clauses are tail call optimized,
                    // so return a maybe ThunkValue
                    return self.node_eval(&*expression, Arc::clone(&frame), allow_thunk);
                }
            }
        }
        return Ok(NULL);
    }

    fn eval_expression_list(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        allow_thunk: bool,
        expressions: Vec<parser::Node>,
    ) -> Result<Value, error::Err> {
        let length = expressions.len();
        if length == 0 {
            return Ok(NULL);
        }
        let call_frame = Arc::new(RwLock::new(StackFrame {
            parent: Some(frame),
            vt: ValueTable::new(),
        }));
        for i in 0..length - 1 {
            self.node_eval(&expressions[i], Arc::clone(&call_frame), false)?;
        }
        // GoInk: return values of expression lists are tail call optimized,
        // so return a maybe ThunkValue
        return self.node_eval(&expressions[length - 1..][0], call_frame, allow_thunk);
    }

    fn eval_object_literal(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        _allow_thunk: bool,
        entries: Vec<parser::Node>,
    ) -> Result<Value, error::Err> {
        let mut obj = ValueTable::new();
        for entry in entries {
            if let parser::Node::ObjectEntryNode { key, val, .. } = entry {
                let key_str = self.operand_to_string(*key, Arc::clone(&frame))?;
                obj.insert(key_str, self.node_eval(&val, Arc::clone(&frame), false)?);
            }
        }
        Ok(Value::CompositeValue(obj))
    }

    fn eval_list_literal(
        &self,
        frame: Arc<RwLock<StackFrame>>,
        _allow_thunk: bool,
        vals: Vec<parser::Node>,
    ) -> Result<Value, error::Err> {
        let mut list_val = ValueTable::new();
        for (i, n) in vals.iter().enumerate() {
            list_val.insert(
                i.to_string(),
                self.node_eval(&n, Arc::clone(&frame), false)?,
            );
        }
        Ok(Value::CompositeValue(list_val))
    }

    // GoInk: log_err logs an Err (interpreter error) according to the configurations
    // specified in the Context's Engine.
    pub fn log_err(&self, e: error::Err) {
        let mut msg = e.message;
        if self.file != "" {
            msg = msg + " in " + &self.file;
        }

        if self.engine.read().unwrap().fatal_error {
            log::log_err(e.reason, &[msg])
        } else {
            log::log_safe_err(e.reason, &[msg])
        }
    }

    // GoInk: dump prints the current state of the Context's global heap
    fn dump(&self) {
        log::log_debug(&[
            "frame dump".to_string(),
            self.frame.read().unwrap().to_string(),
        ])
    }

    // GoInk: exec runs an Ink program.
    // This is the main way to invoke Ink programs from Rust.
    // exec blocks until the Ink program exits.
    pub async fn exec(&mut self, source: &[&str]) -> Result<Value, error::Err> {
        let dump = self.engine.read().unwrap().debug.dump;
        let tokens: &mut Vec<lexer::Tok> = &mut Vec::new();
        lexer::tokenize(tokens, source, true, true);
        let nodes = parser::parse(tokens, true, true);

        let sync_result = Ok(self.eval(nodes, dump));
        let mut latest_async_result: Option<Result<Value, error::Err>> = None;
        if self.engine.write().unwrap().listeners.load(SeqCst) == 0 {
            return sync_result;
        }
        while let Some(message) = self.event_channel.1.recv().await {
            let eng = self.engine.write().unwrap();
            let eval_lock = eng.eval_lock.lock().unwrap();

            match message {
                Message::InkFunctionCallback(ink_func) => {
                    latest_async_result =
                        Some(self.eval_ink_function(ink_func.0, ink_func.1, ink_func.2))
                }
                Message::Result(result) => {
                    if let Err(err) = result {
                        if let error::Err { reason, message } = err {
                            self.log_err(error::Err {
                                reason: reason,
                                message: message,
                            });
                        } else {
                            self.log_err(error::Err {
                                reason: error::ERR_ASSERT,
                                message: "Eval of an Ink node returned error not of type Err"
                                    .to_string(),
                            });
                        }
                    } else {
                        latest_async_result = Some(result);
                    }
                }
            }

            if eng.listeners.fetch_sub(1, SeqCst) == 1 {
                if let Some(async_result) = latest_async_result {
                    return async_result;
                } else {
                    return Err(error::Err {
                        reason: error::ERR_RUNTIME,
                        message: "Missing result in event loop!".to_string(),
                    });
                }
            }
            drop(eval_lock);
            drop(eng);
        }
        sync_result
    }

    // GoInk: exec_path is a convenience function to exec() a program file in a given Context.
    pub async fn exec_path(&mut self, file_path: String) -> Result<Value, error::Err> {
        // update cwd for any potential load() calls this file will make
        self.cwd = Path::new(&file_path)
            .parent()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        self.file = file_path;

        let file_bytes = fs::read(self.file.to_string());
        if let Err(err) = file_bytes {
            let system_err = error::Err {
                reason: error::ERR_SYNTAX,
                message: format!(
                    "could not open {} for execution:\n\t-> {}",
                    self.file.to_string(),
                    err
                ),
            };
            log::log_safe_err(system_err.reason, &[system_err.message.to_string()]);
            return Err(system_err);
        }
        let _file_bytes = file_bytes.unwrap();
        let file_utf8 = str::from_utf8(&_file_bytes).unwrap();
        let file_unicode = UnicodeSegmentation::graphemes(file_utf8, true).collect::<Vec<&str>>();

        self.exec(&file_unicode).await
    }
}

impl fmt::Display for StackFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entries: Vec<String> = Vec::new();
        for (key, val) in &self.vt {
            let mut vstr: String = val.to_string();
            if vstr.len() > MAX_PRINT_LEN {
                vstr = [vstr[MAX_PRINT_LEN..].to_string(), "..".to_string()].join("")
            }
            entries.push(format!("{} -> {}", key, vstr))
        }

        if let Some(parent) = &self.parent {
            return write!(
                f,
                "{{\n\t{}\n}} -prnt-> {}",
                entries.join("\n\t"),
                parent.read().unwrap()
            );
        }
        return write!(f, "{{\n\t{}\n}} -prnt-> {}", entries.join("\n\t"), "*");
    }
}

// GoInk: Engine is a single global context of Ink program execution.
//
// A single thread of execution may run within an Engine at any given moment,
// and this is ensured by an internal execution lock. An execution's Engine
// also holds all permission and debugging flags.
//
// Within an Engine, there may exist multiple Contexts that each contain different
// execution environments, running concurrently under a single lock.
pub struct Engine {
    // Listeners keeps track of the concurrent threads of execution running
    // in the Engine. Call `Engine.listeners.wait()` to block until all concurrent
    // execution threads finish on an Engine.
    pub listeners: AtomicI32,

    // If fatal_error is true, an error will halt the interpreter
    pub fatal_error: bool,
    pub permissions: PermissionsConfig,
    pub debug: DebugConfig,

    // Ink de-duplicates imported source files here, where
    // Contexts from imports are deduplicated keyed by the
    // canonicalized import path. This prevents recursive
    // imports from crashing the interpreter and allows other
    // nice functionality.
    pub contexts: HashMap<String, Context>,

    // Only a single function may write to the stack frames
    // at any moment.
    pub eval_lock: Mutex<bool>,
}

// GoInk: Context represents a single, isolated execution context with its global heap,
// imports, call stack, and cwd (working directory).
pub struct Context {
    // cwd is an always-absolute path to current working dir (of module system)
    cwd: String,
    // currently executing file's path, if any
    file: String,
    pub engine: Arc<RwLock<Engine>>,
    // frame represents the Context's global heap
    pub frame: Arc<RwLock<StackFrame>>,
    // the results of async events
    pub event_channel: (
        tokio::sync::mpsc::UnboundedSender<Message>,
        tokio::sync::mpsc::UnboundedReceiver<Message>,
    ),
}

// GoInk: PermissionsConfig defines Context's permissions to
// operating system interfaces
pub struct PermissionsConfig {
    pub read: bool,
    pub write: bool,
    pub net: bool,
    pub exec: bool,
}

// GoInk: DebugConfig defines any debugging flags referenced at runtime
pub struct DebugConfig {
    pub lex: bool,
    pub parse: bool,
    pub dump: bool,
}
