/*

https://oj.moonbitlang.com/problems/202412-001-find-the-unique-number

*/

fn solution(nums: Vec<i32>) -> i32 {
    nums.iter().fold(0, |acc, x| acc ^ x)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solution() {
        assert_eq!(4, solution(vec![1, 1, 2, 2, 3, 3, 4, 5, 5]));
        assert_eq!(2, solution(vec![0, 1, 0, 1, 2]));
        assert_eq!(10, solution(vec![7, 3, 3, 7, 10]));
    }
}
