/*
    cargo watch -x test

    https://github.com/mocobeta/rust99/blob/master/working-with-vectors/README.md
*/

fn main() {
    println!("Hello, World!");
}

pub fn last<T>(lst: &Vec<T>) -> Option<&T> {
    if lst.is_empty() {
        None
    } else {
        Some(&lst[lst.len() - 1])
    }
}

pub fn last2<T>(lst: &Vec<T>) -> Option<&T> {
    lst.last()
}

mod tests {
    use super::*;

    #[test]
    fn test_last() {
        assert_eq!(None, last::<isize>(&vec![]));
        assert_eq!(Some(&1), last(&vec![1]));
        assert_eq!(Some(&2), last(&vec![1, 2]));

        assert_eq!(None, last2::<isize>(&vec![]));
        assert_eq!(Some(&1), last2(&vec![1]));
        assert_eq!(Some(&2), last2(&vec![1,2]));
    }
}
