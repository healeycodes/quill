use crate::error;
use crate::lexer;
use crate::log;
use crate::parser;
use std::{
	cmp,
	collections::HashMap,
	fmt, fs, io, str,
	sync::{Arc, Barrier, Mutex},
	thread,
};

use unicode_segmentation::UnicodeSegmentation;

const MAX_PRINT_LEN: usize = 120;

// GoInk: Value represents any value in the Ink programming language.
// Each value corresponds to some primitive or object value created
// during the execution of an Ink program.
#[derive(Debug)]
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
}

#[derive(Debug)]
struct FunctionValue {
	defn: parser::Node, // FunctionLiteralNode
	parent_frame: StackFrame,
}

#[derive(Debug)]
struct FunctionCallThunkValue {
	vt: ValueTable,
	function: FunctionValue,
}

// GoInk: The singleton Null value is interned into a single value
const NULL: Value = Value::NullValue(0);

impl fmt::Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Value::EmptyValue {} => write!(f, "_"),
			Value::NumberValue(n) => write!(f, "{}", nv_to_s(*n)),
			Value::StringValue(s) => {
				write!(
					f,
					"'{}'",
					String::from_utf8(s.to_owned())
						.unwrap()
						.replace("\\", "\\\\")
						.replace("'", "\\'")
				)
			}
			Value::BooleanValue(b) => write!(f, "{}", b),
			Value::NullValue(_) => write!(f, "()"),
			Value::CompositeValue(vt) => {
				let mut entries: Vec<String> = Vec::new();
				for (key, value) in vt {
					entries.push(format!("{}: {}", key, value))
				}
				write!(f, "{{{}}}", entries.join(", "))
			}
			Value::FunctionValue(fv) => {
				write!(f, "{}", function_value_to_string(&fv))
			}
			Value::FunctionCallThunkValue(ft) => {
				write!(f, "Thunk of ({})", function_value_to_string(&ft.function))
			}
			_ => write!(f, "TODO"),
		}
	}
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
				Value::BooleanValue(b) => {
					if let Value::BooleanValue(o) = other {
						*b as bool == *o as bool
					} else {
						false
					}
				}
				Value::NullValue(b) => {
					if let Value::NullValue(o) = other {
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
				_ => false,
			}
		}
	}
}

fn operand_to_string(
	right_operand: parser::Node,
	frame: &StackFrame,
) -> Result<String, error::Err> {
	match right_operand {
		parser::Node::IdentifierNode { val, .. } => return Ok(val),
		parser::Node::StringLiteralNode { val, .. } => return Ok(val),
		parser::Node::NumberLiteralNode { val, .. } => return Ok(n_to_s(val)),
		_ => {
			let right_evaluated_value = right_operand.eval(&mut frame, false)?;
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

fn eval_unary(
	frame: &StackFrame,
	allow_thunk: bool,
	operator: &lexer::Kind,
	operand: &Box<parser::Node>,
	position: &lexer::Position,
) -> Result<Value, error::Err> {
	let operand = operand.eval(&mut frame, false);
	if let Err(err) = operand {
		return Err(err);
	} else if !matches!(operator, lexer::Token::NegationOp) {
		let assert_err = error::Err {
			reason: error::ERR_ASSERT,
			message: format!("unrecognized unary operator {}", operator),
		};
		log::log_err_f(assert_err.reason, &[assert_err.message]);
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
	frame: &mut StackFrame,
	allow_thunk: bool,
	operator: &lexer::Kind,
	left_operand: &Box<parser::Node>,
	right_operand: &Box<parser::Node>,
	position: &lexer::Position,
) -> Result<Value, error::Err> {
	if matches!(operator, lexer::Token::DefineOp) {
		if let parser::Node::IdentifierNode { val, position, .. } = **left_operand {
			if let parser::Node::EmptyIdentifierNode { .. } = **right_operand {
				return Err(error::Err {
					reason: error::ERR_RUNTIME,
					message: format!(
						"cannot assign an empty identifier value to {} [{}]",
						val, position
					),
				});
			} else {
				let right_value = right_operand.eval(frame, false);
				frame.set(val, right_value?);
				return right_value;
			}
		}
		if let parser::Node::BinaryExprNode {
			operator,
			left_operand,
			right_operand,
			position,
			..
		} = **left_operand
		{
			if operator == lexer::Token::AccessorOp {
				let left_value = left_operand.eval(frame, false)?;
				let left_key = operand_to_string(*right_operand, frame)?;
				if let Value::CompositeValue(vt) = left_value {
					let right_value = right_operand.eval(frame, false)?;
					vt[&left_key] = right_value;
					return Ok(Value::CompositeValue(vt));
				} else if let Value::StringValue(mut left_string) = left_value {
					if let parser::Node::IdentifierNode { val, .. } = *left_operand {
						let right_value = right_operand.eval(frame, false)?;
						if let Value::StringValue(mut right_string) = right_value {
							let right_num = left_key.parse::<i64>();
							if let Err(right_num) = right_num {
								return Err(error::Err{
									reason: error::ERR_RUNTIME,
									message: format!("while accessing string {} at an index, found non-integer index {} [{}]",
									Value::StringValue(left_string), left_key, right_operand.pos()
								)
								});
							}
							let rn = right_num.unwrap() as usize;
							if 0 <= rn && rn < left_string.len() {
								for (i, r) in left_string.iter().enumerate() {
									if rn + i < left_string.len() {
										left_string[rn + i] = *r
									} else {
										left_string.push(*r)
									}
								}
								frame.up(val, Value::StringValue(left_string));
								return Ok(Value::StringValue(left_string));
							} else if rn == left_string.len() {
								left_string.append(&mut right_string);
								frame.up(val, Value::StringValue(left_string));
								return Ok(Value::StringValue(left_string));
							} else {
								return Err(error::Err {
									reason: error::ERR_RUNTIME,
									message: format!(
										"tried to modify string {} at out of bounds index {} [{}]",
										Value::StringValue(left_string),
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
		let left_value = left_operand.eval(frame, false)?;
		let right_value_str = operand_to_string(**right_operand, frame)?;
		if let Value::CompositeValue(left_value_composite) = left_value {
			if !left_value_composite.contains_key(&right_value_str) {
				return Ok(NULL);
			}
			return Ok(*left_value_composite.get(&right_value_str).unwrap());
		} else if let Value::StringValue(left_string) = left_value {
			let right_num = right_value_str.parse::<i64>();
			if let Err(right_num) = right_num {
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
			if 0 <= rn && rn < left_string.len() {
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

	let left_value = left_operand.eval(frame, false)?;
	let right_value = right_operand.eval(frame, false)?;
	match operator {
		lexer::Token::AddOp => {
			match left_value {
				Value::NumberValue(left) => {
					if let Value::NumberValue(right) = right_value {
						return Ok(Value::NumberValue(left + right));
					}
				}
				Value::StringValue(left) => {
					if let Value::StringValue(right) = right_value {
						// GoInk: In this context, strings are immutable. i.e. concatenating
						// strings should produce a completely new string whose modifications
						// won't be observable by the original strings.
						return Ok(Value::StringValue([left, right].concat()));
					}
				}
				Value::BooleanValue(left) => {
					if let Value::BooleanValue(right) = right_value {
						return Ok(Value::BooleanValue(left || right));
					}
				}
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
							message: format!("division by zero error [{}]", right_operand.pos()),
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
				Value::StringValue(left) => {
					if let Value::StringValue(right) = right_value {
						let max = max_len(&left, &right);
						let a = zero_extend(left, max);
						let b = zero_extend(right, max);
						let mut c: Vec<u8> = Vec::new();
						for i in 0..max {
							c[i] = a[i] & b[i]
						}
						return Ok(Value::StringValue(c));
					}
				}
				Value::BooleanValue(left) => {
					if let Value::BooleanValue(right) = right_value {
						return Ok(Value::BooleanValue(left && right));
					}
				}
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
				Value::NumberValue(left) => {
					if let Value::NumberValue(right) = right_value {
						if is_intable(Value::NumberValue(left))
							&& is_intable(Value::NumberValue(right))
						{
							return Ok(Value::NumberValue((left as i64 | right as i64) as f64));
						}

						return Err(error::Err {
							reason: error::ERR_RUNTIME,
							message: format!(
								"cannot take logical | of non-integer values {}, {} [{}]",
								nv_to_s(right),
								nv_to_s(left),
								right_operand.pos()
							),
						});
					}
				}
				Value::StringValue(left) => {
					if let Value::StringValue(right) = right_value {
						let max = max_len(&left, &right);
						let a = zero_extend(left, max);
						let b = zero_extend(right, max);
						let mut c: Vec<u8> = Vec::new();
						for i in 0..max {
							c[i] = a[i] | b[i]
						}
						return Ok(Value::StringValue(c));
					}
				}
				Value::BooleanValue(left) => {
					if let Value::BooleanValue(right) = right_value {
						return Ok(Value::BooleanValue(left || right));
					}
				}
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
				Value::NumberValue(left) => {
					if let Value::NumberValue(right) = right_value {
						if is_intable(Value::NumberValue(left))
							&& is_intable(Value::NumberValue(right))
						{
							return Ok(Value::NumberValue((left as i64 ^ right as i64) as f64));
						}

						return Err(error::Err {
							reason: error::ERR_RUNTIME,
							message: format!(
								"cannot take logical ^ of non-integer values {}, {} [{}]",
								nv_to_s(right),
								nv_to_s(left),
								right_operand.pos()
							),
						});
					}
				}
				Value::StringValue(left) => {
					if let Value::StringValue(right) = right_value {
						let max = max_len(&left, &right);
						let a = zero_extend(left, max);
						let b = zero_extend(right, max);
						let mut c: Vec<u8> = Vec::new();
						for i in 0..max {
							c[i] = a[i] & b[i]
						}
						return Ok(Value::StringValue(c));
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
					"values {} and {} do not support bitwise or logical ^ [{}]",
					left_value,
					right_value,
					right_operand.pos()
				),
			});
		}
		lexer::Token::GreaterThanOp => {
			match left_value {
				Value::NumberValue(left) => {
					if let Value::NumberValue(right) = right_value {
						return Ok(Value::BooleanValue(left > right));
					}
				}
				Value::StringValue(left) => {
					if let Value::StringValue(right) = right_value {
						return Ok(Value::BooleanValue(left > right));
					}
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
		lexer::Token::LessThanOp => {
			if let Value::NumberValue(left) = left_value {
				if let Value::NumberValue(right) = right_value {
					return Ok(Value::BooleanValue(left < right));
				}
			} else if let Value::StringValue(left) = left_value {
				if let Value::StringValue(right) = right_value {
					return Ok(Value::BooleanValue(left < right));
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
		_ => {
			let assert_err = error::Err {
				reason: error::ERR_ASSERT,
				message: format!("unknown binary operator {}", operator),
			};
			log::log_err_f(assert_err.reason, &[assert_err.message.clone()]);
			return Err(assert_err);
		}
	}
}

impl parser::Node {
	fn eval(&self, frame: &mut StackFrame, allow_thunk: bool) -> Result<Value, error::Err> {
		if matches!(self, parser::Node::EmptyIdentifierNode { .. }) {
			Ok(Value::EmptyValue {})
		} else {
			match &*self {
				parser::Node::UnaryExprNode {
					operator,
					operand,
					position,
					..
				} => return eval_unary(frame, allow_thunk, operator, operand, position),
				parser::Node::BinaryExprNode {
					operator,
					left_operand,
					right_operand,
					position,
					..
				} => {
					return eval_binary(
						frame,
						allow_thunk,
						operator,
						left_operand,
						right_operand,
						position,
					)
				}
				parser::Node::NumberLiteralNode { val, .. } => Ok(Value::NumberValue(*val)),
				_ => Ok(Value::EmptyValue {}),
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
fn nv_to_s(v: f64) -> String {
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
#[derive(Debug)]
struct StackFrame {
	pub parent: Option<Box<StackFrame>>,
	pub vt: ValueTable,
}

// unwrapThunk expands out a recursive structure of thunks
// 	into a flat for loop control structure
// fn unwrap_thunk(thunk: FunctionCallThunkValue) -> Result<(), error::Err> {
// 	let mut is_thunk = true;
// 	while is_thunk {
// 		let frame = &StackFrame{
// 			parent: thunk.function.parent_frame,
// 			vt:     thunk.vt,
// 		};
// 		let (v, err) = thunk.function.defn.body.Eval(frame, true)
//         if let Err(err) = err {
//             return (Err(err), 0);
//         }
// 		let (thunk, is_thunk) = v.(FunctionCallThunkValue)
// 	}

// 	return
// }

impl StackFrame {
	// GoInk: Set a value to the most recent call stack frame
	fn set(&mut self, name: String, val: Value) {
		self.vt.insert(name, val);
	}
	// GoInk: Up updates a value in the stack frame chain
	fn up(&mut self, name: String, val: Value) {
		if self.vt.contains_key(&name) {
			self.vt.insert(name, val);
			return;
		}
		if let Some(parent) = &mut self.parent {
			parent.up(name, val)
		}
	}
}

impl Engine<'_> {
	// GoInk: create_contex creates and initializes a new Context tied to a given Engine.
	pub fn create_context(&self) -> Context {
		let ctx = Context {
			cwd: String::new(),
			file: String::new(),
			engine: self,
			frame: StackFrame {
				parent: Option::None,
				vt: HashMap::new(),
			},
		};

		// If first time creating Context in this Engine,
		// initialize the Contexts map
		// TODO: how to check if HashMap is initialized in rust?
		// if eng.contexts == nil {
		// eng.contexts = HashMap::new();
		// }

		// ctx.reset_wd()
		// ctx.load_environment()

		ctx
	}
}

// GoInk: eval takes a channel of Nodes to evaluate, and executes the Ink programs defined
// in the syntax tree. eval returns the last value of the last expression in the AST,
// or an error if there was a runtime error.
impl Context<'_> {
	fn eval(&mut self, nodes: Vec<parser::Node>, dump_frame: bool) -> Value {
		self.engine.eval_lock.lock().unwrap();
		let mut result = Value::EmptyValue {};
		for node in nodes.iter() {
			let val = node.eval(&mut self.frame, false);
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

	// GoInk: exec_listener queues an asynchronous callback task to the Engine behind the Context.
	// Callbacks registered this way will also run with the Engine's execution lock.
	// fn exec_listener(&self, callback: fn()) {
	// 	self.engine.listeners
	// 	// ctx.Engine.Listeners.Add(1)
	// 	go func() {
	// 		defer ctx.Engine.Listeners.Done()

	// 		ctx.Engine.evalLock.Lock()
	// 		defer ctx.Engine.evalLock.Unlock()

	// 		callback()
	// 	}()
	// }

	// GoInk: log_err logs an Err (interpreter error) according to the configurations
	// specified in the Context's Engine.
	fn log_err(&self, e: error::Err) {
		let mut msg = e.message;
		if self.file != "" {
			msg = msg + " in " + &self.file;
		}

		if self.engine.fatal_error {
			log::log_err(e.reason, &[msg])
		} else {
			log::log_safe_err(e.reason, &[msg])
		}
	}

	// GoInk: dump prints the current state of the Context's global heap
	fn dump(&self) {
		log::log_debug(&["frame dump".to_string(), self.frame.to_string()])
	}

	// GoInk: exec runs an Ink program.
	// This is the main way to invoke Ink programs from Go.
	// exec blocks until the Ink program exits.
	pub fn exec(&mut self, source: &[&str]) -> Result<Value, error::Err> {
		let eng = self.engine;

		let tokens: &mut Vec<lexer::Tok> = &mut Vec::new();
		lexer::tokenize(tokens, source, true, true);
		let nodes = parser::parse(tokens, true, true);

		// TODO: surely tokenizing or parsing can cause errors we should raise
		return Ok(self.eval(nodes, eng.debug.dump));
	}

	// GoInk: exec_path is a convenience function to exec() a program file in a given Context.
	pub fn exec_path(&mut self, file_path: String) -> Result<Value, error::Err> {
		// update cwd for any potential load() calls this file will make
		// self.cwd = path.dir(file_path);
		self.file = file_path;

		// TODO: add a 'could not open' log message
		let file_bytes = fs::read(self.file.clone());
		if let Err(err) = file_bytes {
			let system_err = error::Err {
				reason: error::ERR_SYNTAX,
				message: format!(
					"could not open {} for execution:\n\t-> {}",
					self.file.clone(),
					err
				),
			};
			log::log_safe_err(system_err.reason, &[system_err.message.clone()]);
			return Err(system_err);
		}
		let _file_bytes = file_bytes.unwrap();
		let file_utf8 = str::from_utf8(&_file_bytes).unwrap();
		let file_unicode = UnicodeSegmentation::graphemes(file_utf8, true).collect::<Vec<&str>>();

		self.exec(&file_unicode)
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

		return write!(
			f,
			"{{\n\t{}\n}} -prnt-> {}",
			entries.join("\n\t"),
			"" // TODO: self.parent
		);
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
pub struct Engine<'a> {
	// Listeners keeps track of the concurrent threads of execution running
	// in the Engine. Call `Engine.listeners.wait()` to block until all concurrent
	// execution threads finish on an Engine.
	pub listeners: Arc<Barrier>,

	// If fatal_error is true, an error will halt the interpreter
	pub fatal_error: bool,
	pub permissions: PermissionsConfig,
	pub debug: DebugConfig,

	// Ink de-duplicates imported source files here, where
	// Contexts from imports are deduplicated keyed by the
	// canonicalized import path. This prevents recursive
	// imports from crashing the interpreter and allows other
	// nice functionality.
	pub contexts: HashMap<String, &'a Context<'a>>,

	// Only a single function may write to the stack frames
	// at any moment.
	pub eval_lock: Mutex<i32>,
}

// GoInk: Context represents a single, isolated execution context with its global heap,
// imports, call stack, and cwd (working directory).
pub struct Context<'a> {
	// cwd is an always-absolute path to current working dir (of module system)
	cwd: String,
	// currently executing file's path, if any
	file: String,
	engine: &'a Engine<'a>,
	// frame represents the Context's global heap
	frame: StackFrame,
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
