fn main() {
    println!("Hello, world!");
}

// gcd a b = if a == 0 then b else gcd (b `mod` a) a
fn gcd_of_strings(str1: String, str2: String) -> String {
    if str1.is_empty() {
        str2
    } else {
        let rest = str2.replacen(&str1, "", 1);
        if rest == str2 {
            "".to_string()
        } else {
            gcd_of_strings(rest.to_string(), str1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    // cspell:disable
    fn test_gcd_of_strings() {
        assert_eq!(
            String::from("ABC"),
            gcd_of_strings(String::from("ABCABC"), String::from("ABC")),
        );
        assert_eq!(
            String::from("AB"),
            gcd_of_strings(String::from("ABABAB"), String::from("ABAB")),
        );
        assert_eq!(
            String::from(""),
            gcd_of_strings(String::from("LEET"), String::from("CODE")),
        );
        assert_eq!(
            String::from(""),
            gcd_of_strings(String::from("ABCDEF"), String::from("ABC")),
        );
        assert_eq!(
            String::from("TAUXX"),
            gcd_of_strings(
                String::from("TAUXXTAUXXTAUXXTAUXXTAUXX"),
                String::from("TAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXX")
            )
        )
    }
    // cspell:enable
}
