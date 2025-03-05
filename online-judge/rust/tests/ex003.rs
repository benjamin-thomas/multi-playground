/*

https://oj.moonbitlang.com/problems/202412-003-number-string-formatting

*/

use rpds::List;

// Haskell-inspired solution (no mutation)
fn solution(s: String) -> String {
    let s2 = strip_leading_zeros(&s);
    if s2.starts_with(".") {
        s
    } else {
        let offset = find_offset(&s2);
        insert_separator(offset, s2)
    }
}

fn find_offset(s: &str) -> usize {
    s.find('.').map(|idx| s.len() - idx).unwrap_or(0)
}

fn strip_leading_zeros(str: &str) -> String {
    str.trim_start_matches('0').to_string()
}

fn insert_separator(offset: usize, s: String) -> String {
    let (_, chars) = s.chars().rfold((0, List::new()), |acc, c| {
        let (n, lst) = acc;
        (
            n + 1,
            if n > offset && n % 3 == offset % 3 {
                lst.push_front(',').push_front(c)
            } else {
                lst.push_front(c)
            },
        )
    });
    chars.iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_offset() {
        assert_eq!(0, find_offset("123456"));
        assert_eq!(2, find_offset("123.4"));
        assert_eq!(3, find_offset("777888999.12"));
    }

    #[test]
    fn test_strip_leading_zeros() {
        assert_eq!("1200", strip_leading_zeros("0001200"));
    }

    #[test]
    fn test_solution() {
        assert_eq!("123", solution(String::from("123")));
        assert_eq!("123.4", solution(String::from("123.4")));
        assert_eq!("123.4", insert_separator(2, String::from("123.4")));

        assert_eq!("1,294,512.12412", solution(String::from("1294512.12412")));
        assert_eq!("123,456,789.99", solution(String::from("0000123456789.99")));
        assert_eq!("987,654,321", solution(String::from("987654321")));

        assert_eq!("0.123", solution(String::from("0.123")));
        assert_eq!(
            "1,234,567.987654321",
            solution(String::from("1234567.987654321"))
        );
    }
}
