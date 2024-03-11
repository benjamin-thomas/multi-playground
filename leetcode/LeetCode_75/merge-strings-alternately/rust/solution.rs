// echo ./solution.rs | entr -c bash -c 'rustc --test ./solution.rs -o /tmp/solution && /tmp/solution --test && echo OK'

fn merge1(a: String, b: String) -> String {
    let mut result = String::with_capacity(a.len() + b.len());
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

fn merge2(a: String, b: String) -> String {
    let mut result = String::with_capacity(a.len() + b.len());
    let mut a = a.chars();
    let mut b = b.chars();
    loop {
        match (a.next(), b.next()) {
            (Some(x), Some(y)) => {
                result.push(x);
                result.push(y);
            }
            (Some(x), None) => {
                result.push(x);
                result.extend(a);
                break;
            }
            (None, Some(y)) => {
                result.push(y);
                result.extend(b);
                break;
            }
            (None, None) => break,
        }
    }
    result
}

fn merge3(a: String, b: String) -> String {
    let mut result = String::with_capacity(a.len() + b.len());
    let merged_chars = a.chars().zip(b.chars()).flat_map(|(x, y)| vec![x, y]);
    result.extend(merged_chars);
    result.extend(a.chars().skip(b.len()));
    result.extend(b.chars().skip(a.len()));
    result
}

#[cfg(test)]
mod tests {
    use merge1;
    use merge2;
    use merge3;

    struct TestCase {
        a: String,
        b: String,
        expected: String,
    }

    impl TestCase {
        fn a(&self) -> &str {
            &self.a
        }
        fn b(&self) -> &str {
            &self.b
        }
        fn expected(&self) -> &str {
            &self.expected
        }
    }

    fn test_cases() -> Vec<TestCase> {
        vec![
            TestCase {
                a: "ABC".to_string(),
                b: "abc".to_string(),
                expected: "AaBbCc".to_string(),
            },
            TestCase {
                a: "AB_CDE".to_string(),
                b: "ab".to_string(),
                expected: "AaBb_CDE".to_string(),
            },
            TestCase {
                a: "AB".to_string(),
                b: "ab_cde".to_string(),
                expected: "AaBb_cde".to_string(),
            },
            TestCase {
                a: "ABC".to_string(),
                b: "".to_string(),
                expected: "ABC".to_string(),
            },
            TestCase {
                a: "".to_string(),
                b: "abc".to_string(),
                expected: "abc".to_string(),
            },
            TestCase {
                a: "".to_string(),
                b: "".to_string(),
                expected: "".to_string(),
            },
        ]
    }

    #[test]
    fn test_merge() {
        for test_case in test_cases() {
            assert_eq!(
                merge1(test_case.a().to_string(), test_case.b().to_string()),
                test_case.expected()
            );

            assert_eq!(
                merge2(test_case.a().to_string(), test_case.b().to_string()),
                test_case.expected()
            );

            assert_eq!(
                merge3(test_case.a().to_string(), test_case.b().to_string()),
                test_case.expected()
            );
        }
    }
}
