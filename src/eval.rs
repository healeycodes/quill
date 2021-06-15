use std::sync::{Arc, Barrier};
use std::thread;

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
	// in the Engine. Call `Engine.Listeners.wait()` to block until all concurrent
	// execution threads finish on an Engine.
	Listeners: Arc<Barrier>,

	// If FatalError is true, an error will halt the interpreter
	FatalError:  bool,
	Permissions: PermissionsConfig,
	Debug     :  DebugConfig,

	// Ink de-duplicates imported source files here, where
	// Contexts from imports are deduplicated keyed by the
	// canonicalized import path. This prevents recursive
	// imports from crashing the interpreter and allows other
	// nice functionality.
	Contexts: map[string]*Context,

	// Only a single function may write to the stack frames
	// at any moment.
	evalLock: sync.Mutex,
}