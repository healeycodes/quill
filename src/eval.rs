use std::collections::HashMap;
use std::sync::{Arc, Barrier, Mutex};
use std::thread;

// GoInk: Value represents any value in the Ink programming language.
// Each value corresponds to some primitive or object value created
// during the execution of an Ink program.
enum Value {}

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

// GoInk: ValueTable is used anytime a map of names/labels to Ink Values is needed,
// and is notably used to represent stack frames / heaps and CompositeValue dictionaries.
type ValueTable = HashMap<String, Value>;

// GoInk: StackFrame represents the heap of variables local to a particular function call frame,
// and recursively references other parent StackFrames internally.
struct StackFrame {
	parent: Box<StackFrame>,
	vt: ValueTable,
}

// GoInk: Eval takes a channel of Nodes to evaluate, and executes the Ink programs defined
// in the syntax tree. Eval returns the last value of the last expression in the AST,
// or an error if there was a runtime error.
impl Context {
	fn eval(&self, nodes: &[lexer::Tok], dump_frame: bool) -> Result<Value, Err> {
		self.engine.eval_lock.lock().unwrap();
		for node in nodes.iter() {
			let (val, err) = node.eval(self.frame, false);
			if let Err(err) = err {
				return (Err(err), 0);
			}
		}
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
