// echo ./solution.rs | entr -c bash -c 'rustc --test ./solution.rs -o /tmp/solution && /tmp/solution --test && echo OK'

fn merge_alternatively(a: String, b: String) -> String {
    let mut result = String::new();
    let mut i = 0;
    let mut j = 0;
    while i < a.len() || j < b.len() {
        if i < a.len() {
            result.push(a.chars().nth(i).unwrap());
            i += 1;
        }
        if j < b.len() {
            result.push(b.chars().nth(j).unwrap());
            j += 1;
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use merge_alternatively;

    #[test]
    fn test() {
        assert_eq!(
            merge_alternatively("ABC".to_string(), "abc".to_string()),
            "AaBbCc".to_string()
        );
    }
}
