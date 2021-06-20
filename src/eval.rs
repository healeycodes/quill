use crate::error;
use crate::lexer;
use crate::log;
use crate::parser;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io;
use std::str;
use std::sync::{Arc, Barrier, Mutex};
use std::thread;

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
	StringValue(String),
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
				write!(f, "'{}'", s.replace("\\", "\\\\").replace("'", "\\'"))
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

impl parser::Node {
	fn eval(&self, frame: &StackFrame, allow_thunk: bool) -> Result<Value, error::Err> {
		if matches!(self, parser::Node::EmptyIdentifierNode { .. }) {
			Ok(Value::EmptyValue {})
		} else {
			match &*self {
				parser::Node::UnaryExprNode {
					operator,
					operand,
					position,
					..
				} => {
					let operand = operand.eval(frame, false);
					if let Err(err) = operand {
						return Err(err);
					}
					let _operand = operand.unwrap();
					match _operand {
						Value::NumberValue(n) => return Ok(Value::NumberValue(-n)),
						Value::BooleanValue(b) => return Ok(Value::BooleanValue(!b)),
						_ => return Err(error::Err {
							reason: error::ERR_RUNTIME,
							message: format!(
								"cannot negate non-boolean and non-number value {} [{}]",
								_operand, position
							),
						}),
					};
					let assert_err = error::Err {
						reason: error::ERR_ASSERT,
						message: format!("unrecognized unary operator {}", &*self),
					};
					log::log_err_f(assert_err.reason, &[assert_err.message]);
					Err(assert_err)
				}
				parser::Node::NumberLiteralNode { val, .. } => Ok(Value::NumberValue(*val)),
				_ => Ok(Value::EmptyValue {}),
			}
		}
	}
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
	fn eval(&self, nodes: Vec<parser::Node>, dump_frame: bool) -> Value {
		self.engine.eval_lock.lock().unwrap();
		let mut result = Value::EmptyValue {};
		for node in nodes.iter() {
			let val = node.eval(&self.frame, false);
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
	pub fn exec(&self, source: &[&str]) -> Result<Value, error::Err> {
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
