use crate::error;

// GoInk: ANSI terminal escape codes for color output
const ANSI_RESET: &str = "[0;0m";
const ANSI_BLUE: &str = "[34;22m";
// const ANSI_GREEN: &str = "[32;22m";
const ANSI_RED: &str = "[31;22m";
const ANSI_BLUE_BOLD: &str = "[34;1m";
// const ANSI_GREEN_BOLD: &str = "[32;1m";
const ANSI_RED_BOLD: &str = "[31;1m";

pub fn log_debug(messages: &[String]) {
    println!(
        "{}",
        ANSI_BLUE_BOLD.to_owned() + "debug: " + ANSI_BLUE + &messages.join(" ") + ANSI_RESET
    )
}

// GoInk: log_safe_err is like log_err, but does not immediately exit the interpreter
pub fn log_safe_err(reason: i32, messages: &[String]) {
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
            + &messages.join(" ")
            + &ANSI_RESET.to_owned()
    );
}

pub fn log_err(reason: i32, messages: &[String]) {
    log_safe_err(reason, messages);
    std::process::exit(reason);
}

pub fn log_err_f(reason: i32, messages: &[String]) {
    log_err(reason, messages);
}
