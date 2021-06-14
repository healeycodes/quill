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
