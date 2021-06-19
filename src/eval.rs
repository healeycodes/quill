use crate::error;
use crate::log;
use crate::parser;
use crate::parser::Position;
use std::collections::HashMap;
use std::fmt;
use std::sync::{Arc, Barrier, Mutex};
use std::thread;

const max_print_len: usize = 120;

// GoInk: Value represents any value in the Ink programming language.
// Each value corresponds to some primitive or object value created
// during the execution of an Ink program.
enum Value {
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

struct FunctionValue {
	defn: parser::Node, // FunctionLiteralNode
	parentFrame: StackFrame,
}

struct FunctionCallThunkValue {
	vt: ValueTable,
	function: FunctionValue,
}

// GoInk: The singleton Null value is interned into a single value
const Null: Value = Value::NullValue(0);

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
	if fstr.len() > max_print_len {
		fstr = [fstr[max_print_len..].to_string(), "..".to_string()].join("")
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
struct StackFrame {
	parent: Box<StackFrame>,
	vt: ValueTable,
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

// GoInk: Eval takes a channel of Nodes to evaluate, and executes the Ink programs defined
// in the syntax tree. Eval returns the last value of the last expression in the AST,
// or an error if there was a runtime error.
impl Context {
	// fn eval(&self, nodes: Vec<parser::Node>, dump_frame: bool) {
	// 	self.engine.eval_lock.lock().unwrap();
	// 	for node in nodes.iter() {
	// 		let (val, err) = node.eval(self.frame, false);
	// 		if let Err(err) = err {
	// 			self.log_err(err);
	// 			break;
	// 		}
	// 	}
	// 	if dump_frame {
	// 		self.dump()
	// 	}
	// }

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

	// GoInk: Dump prints the current state of the Context's global heap
	fn dump(&self) {
		log::log_debug(&["frame dump".to_string(), self.frame.to_string()])
	}
}

impl fmt::Display for StackFrame {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut entries: Vec<String> = Vec::new();
		for (key, val) in &self.vt {
			let mut vstr: String = val.to_string();
			if vstr.len() > max_print_len {
				vstr = [vstr[max_print_len..].to_string(), "..".to_string()].join("")
			}
			entries.push(format!("{} -> {}", key, vstr))
		}

		return write!(
			f,
			"{{\n\t{}\n}} -prnt-> {}",
			entries.join("\n\t"),
			self.parent
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
struct Engine {
	// Listeners keeps track of the concurrent threads of execution running
	// in the Engine. Call `Engine.listeners.wait()` to block until all concurrent
	// execution threads finish on an Engine.
	listeners: Arc<Barrier>,

	// If fatal_error is true, an error will halt the interpreter
	fatal_error: bool,
	permissions: PermissionsConfig,
	debug: DebugConfig,

	// Ink de-duplicates imported source files here, where
	// Contexts from imports are deduplicated keyed by the
	// canonicalized import path. This prevents recursive
	// imports from crashing the interpreter and allows other
	// nice functionality.
	contexts: HashMap<String, Context>,

	// Only a single function may write to the stack frames
	// at any moment.
	eval_lock: Mutex<i32>,
}

// GoInk: Context represents a single, isolated execution context with its global heap,
// imports, call stack, and cwd (working directory).
struct Context {
	// cwd is an always-absolute path to current working dir (of module system)
	cwd: String,
	// currently executing file's path, if any
	file: String,
	engine: Engine,
	// frame represents the Context's global heap
	frame: StackFrame,
}

// GoInk: PermissionsConfig defines Context's permissions to
// operating system interfaces
struct PermissionsConfig {
	read: bool,
	write: bool,
	net: bool,
	exec: bool,
}

// GoInk: DebugConfig defines any debugging flags referenced at runtime
struct DebugConfig {
	Lex: bool,
	Parse: bool,
	Dump: bool,
}
