// ANSI terminal escape codes for color output
const AnsiReset: &str = "[0;0m";
const AnsiBlue: &str = "[34;22m";
const AnsiGreen: &str = "[32;22m";
const AnsiRed: &str = "[31;22m";
const AnsiBlueBold: &str = "[34;1m";
const AnsiGreenBold: &str = "[32;1m";
const AnsiRedBold: &str = "[31;1m";

pub fn log_debug(s1: String, s2: String) {
    print!("{} {}", s1, s2)
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

// // LogSafeErr is like LogErr, but does not immediately exit the interpreter
// func LogSafeErr(reason int, args ...string) {
// 	errStr := "error"
// 	switch reason {
// 	case ErrSyntax:
// 		errStr = "syntax error"
// 	case ErrRuntime:
// 		errStr = "runtime error"
// 	case ErrSystem:
// 		errStr = "system error"
// 	case ErrAssert:
// 		errStr = "invariant violation"
// 	default:
// 		errStr = "error"
// 	}
// 	fmt.Fprintln(os.Stderr, AnsiRedBold+errStr+": "+AnsiRed+strings.Join(args, " ")+AnsiReset)
// }

// func LogErr(reason int, args ...string) {
// 	LogSafeErr(reason, args...)
// 	os.Exit(reason)
// }

// func LogErrf(reason int, s string, args ...interface{}) {
// 	LogErr(reason, fmt.Sprintf(s, args...))
// }
