use crate::error;

// GoInk: ANSI terminal escape codes for color output
const ANSI_RESET: &str = "[0;0m";
const ANSI_BLUE: &str = "[34;22m";
const ANSI_GREEN: &str = "[32;22m";
const ANSI_RED: &str = "[31;22m";
const ANSI_BLUE_BOLD: &str = "[34;1m";
const ANSI_GREEN_BOLD: &str = "[32;1m";
const ANSI_RED_BOLD: &str = "[31;1m";

pub fn log_debug(s1: String, s2: String) {
    println!("{} {}", s1, s2)
}

// func LogDebugf(s string, args ...interface{}) {
// 	LogDebug(fmt.Sprintf(s, args...))
// }

// func LogInteractive(args ...string) {
// 	fmt.Println(AnsiGreen + strings.Join(args, " ") + AnsiReset)
// }

// func LogInteractivef(s string, args ...interface{}) {
// 	LogInteractive(fmt.Sprintf(s, args...))
// }

// GoInk: LogSafeErr is like LogErr, but does not immediately exit the interpreter
pub fn log_safe_err(reason: i32, message: &str) {
    let err_str = match reason {
        error::ERR_SYNTAX => String::from("syntax error"),
        error::ERR_RUNTIME => String::from("runtime error"),
        error::ERR_SYSTEM => String::from("system error"),
        error::ERR_ASSERT => String::from("invariant violation"),
        _ => String::from("error"),
    };
    eprintln!(
        "{}",
        ANSI_RED_BOLD.to_owned()
            + &err_str
            + ": "
            + &ANSI_RED.to_owned()
            + &message
            + &ANSI_RESET.to_owned()
    );
}

pub fn log_err(reason: i32, message: &str) {
    log_safe_err(reason, message);
    std::process::exit(reason);
}

// func LogErrf(reason int, s string, args ...interface{}) {
// 	LogErr(reason, fmt.Sprintf(s, args...))
// }
