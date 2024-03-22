fn filter_nums(nums: &str) -> Vec<u32> {
    nums.chars().filter_map(|char| char.to_digit(10)).collect()
}

fn keep_outer(nums: &[u32]) -> Option<(u32, u32)> {
    match (nums.first(), nums.last()) {
        (Some(first), Some(last)) => Some((*first, *last)),
        (Some(first), None) => Some((*first, *first)),
        (None, Some(last)) => Some((*last, *last)),
        (None, None) => None,
    }
}

fn filter_map(lines: Vec<&str>) -> Vec<(u32, u32)> {
    lines
        .iter()
        .map(|line| filter_nums(line))
        .filter_map(|nums| keep_outer(&nums))
        .collect()
}

pub(crate) fn part_1(lines: Vec<&str>) -> u32 {
    filter_map(lines)
        .iter()
        .map(|(tens, units)| tens * 10 + units)
        .sum()
}

pub(crate) fn part_2(lines: Vec<&str>) -> u32 {
    lines
        .iter()
        .map(|line| filter_more_nums(line))
        .filter_map(|nums| keep_outer(&nums))
        .collect::<Vec<(u32, u32)>>()
        .iter()
        .map(|(tens, units)| tens * 10 + units)
        .sum()
}

fn filter_more_nums(nums: &str) -> Vec<u32> {
    let mut res: Vec<u32> = Vec::new();
    for (i, char) in nums.chars().enumerate() {
        if let Some(n) = char.to_digit(10) {
            res.push(n);
        } else if Some("one") == nums.get(i..i + 3) {
            res.push(1);
        } else if Some("two") == nums.get(i..i + 3) {
            res.push(2);
        } else if Some("three") == nums.get(i..i + 5) {
            res.push(3);
        } else if Some("four") == nums.get(i..i + 4) {
            res.push(4);
        } else if Some("five") == nums.get(i..i + 4) {
            res.push(5);
        } else if Some("six") == nums.get(i..i + 3) {
            res.push(6);
        } else if Some("seven") == nums.get(i..i + 5) {
            res.push(7);
        } else if Some("eight") == nums.get(i..i + 5) {
            res.push(8);
        } else if Some("nine") == nums.get(i..i + 4) {
            res.push(9);
        }
    }
    return res;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filter_nums() {
        assert_eq!(filter_nums("1abc2pqr3stu8vwx"), vec![1, 2, 3, 8]);
    }

    #[test]
    fn test_keep_outer() {
        assert_eq!(keep_outer(&[]), None);
        assert_eq!(keep_outer(&[1]), Some((1, 1)));
        assert_eq!(keep_outer(&[1, 2, 3]), Some((1, 3)));
    }

    #[test]
    fn test_filter_map() {
        assert_eq!(
            filter_map(vec!["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]),
            vec![(1, 2), (3, 8), (1, 5), (7, 7)]
        );
    }

    #[test]
    fn test_part_1() {
        assert_eq!(
            part_1(vec!["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]),
            142
        );
    }

    #[test]
    fn test_filter_more_nums() {
        assert_eq!(filter_more_nums("abcone2threeightxyz"), vec![1, 2, 3, 8]);
        assert_eq!(
            filter_more_nums("1.abc.oneight--nineightseveninessixfivefourthreetwone0123456789"),
            vec![1, 1, 8, 9, 8, 7, 9, 6, 5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        );
    }
}
