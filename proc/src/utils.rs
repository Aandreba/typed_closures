pub fn to_pascal_case (s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut is_upper = true;

    for c in s.chars() {
        if c == '_' {
            is_upper = true;
            continue;
        }

        if is_upper {
            result.extend(c.to_uppercase());
            is_upper = false;
            continue;
        }

        result.push(c);
    }

    return result;
}