mod day_01;

/*

rg --files -t rust | entr -c cargo check
cargo run < ../_inputs/day01.txt
rg --files -t rust | entr -c bash -c 'cargo run < ../_inputs/day01.txt'

*/

fn main() {
    // Not sure how to fix that
    let lines1: Vec<String> = std::io::stdin().lines().map(|line| line.unwrap()).collect();
    let lines2: Vec<&str> = lines1.iter().map(|line| line.as_str()).collect();

    println!("Part 1: {}", day_01::part_1(lines2.clone()));
    println!("Part 2: {}", day_01::part_2(lines2.clone()));
}
