// Run with: bacon
fn main() {
    let file = "../_inputs/01.txt";
    let input = std::fs::read_to_string(file).unwrap();
    println!(
        "Answer 1: {} (ok: {})",
        answer1(&input),
        answer1(&input) == 1189304
    );
    println!(
        "Answer 2: {} (ok: {})",
        answer2(&input),
        answer2(&input) == 24349736
    );
}

#[allow(dead_code)]
const EXAMPLE: &str = "
3   4
4   3
2   5
1   3
3   9
3   3
";

/*
Run with one of:

bacon test

To always show print calls:
cargo test -- --nocapture
bacon test -- -- --nocapture

---

Or just:

bacon

Then use 't' to switch to tests
Then use 'r' to run the program
Then use 'c' to run clippy
Then use 'd' to open the docs

 */
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_transforms_the_input() {
        assert_eq!(
            vec![(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)],
            to_tup2(EXAMPLE)
        );

        assert_eq!(
            (vec![3, 4, 2, 1, 3, 3], vec![4, 3, 5, 3, 9, 3]),
            unzip(to_tup2(EXAMPLE))
        );

        assert_eq!(
            (vec![(3, 1), (3, 2), (3, 3), (4, 3), (5, 3), (9, 4)]),
            by_pair_asc(unzip(to_tup2(EXAMPLE)))
        );

        assert_eq!(11, answer1(EXAMPLE));
        assert_eq!(31, answer2(EXAMPLE));
    }
}

fn answer2(input: &str) -> u32 {
    let lst = to_tup2(input).into_iter().unzip();
    let xs: Vec<u32> = lst.0;
    let ys: Vec<u32> = lst.1;
    let tmp: Vec<(u32, u32)> = xs
        .clone()
        .into_iter()
        .map(|x| {
            let occurrences = ys.clone().into_iter().filter(|&y| y == x).count() as u32;
            (x, x * occurrences)
        })
        .collect();
    // print!("xs={:?}\n", xs);
    // print!("tmp={:?}\n", tmp);
    tmp.into_iter().fold(0, |acc, (_, b)| acc + b)
}

fn answer1(input: &str) -> u32 {
    by_pair_asc(unzip(to_tup2(input)))
        .into_iter()
        .fold(0, |acc, (a, b)| acc + (a - b))
}

fn by_pair_asc(input: (Vec<u32>, Vec<u32>)) -> Vec<(u32, u32)> {
    let mut result = Vec::new();
    let mut xs = input.0;
    let mut ys = input.1;
    xs.sort();
    ys.sort();

    for i in 0..xs.len() {
        let a = xs[i];
        let b = ys[i];
        if a > b {
            result.push((xs[i], ys[i]));
        } else {
            result.push((ys[i], xs[i]));
        }
    }
    result
}

fn to_tup2(input: &str) -> Vec<(u32, u32)> {
    input
        .trim()
        .lines()
        .flat_map(|line| {
            line.split_whitespace()
                .filter_map(|s| s.parse().ok())
                .collect::<Vec<u32>>()
                .chunks(2)
                .map(|chunk| (chunk[0], chunk[1]))
                .collect::<Vec<(u32, u32)>>()
        })
        .collect()
}

fn unzip(tups: Vec<(u32, u32)>) -> (Vec<u32>, Vec<u32>) {
    let (xs, ys): (Vec<u32>, Vec<u32>) = tups.into_iter().unzip();
    (xs, ys)
}
