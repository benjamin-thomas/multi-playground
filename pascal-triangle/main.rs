/*

echo ./main.rs | entr -c cargo run
echo ./main.rs | entr -c cargo test

*/

#[test]
fn test_zip_2_arrays() {
    let xs: [u32; 3] = [1, 2, 3];
    let ys: [u32; 3] = [4, 5, 6];
    let zipped = xs.into_iter().zip(ys.into_iter()).collect::<Vec<_>>();
    assert_eq!(zipped, [(1, 4), (2, 5), (3, 6)])
}

#[test]
fn test_sum_pairs() {
    let xs: [u32; 4] = [1, 3, 3, 1];
    let ys: Vec<u32> = xs
        .windows(2)
        .map(|window| window.iter().sum::<u32>())
        .collect();
    assert_eq!(ys, [4, 6, 4]);

    assert_eq!(next_row(&[]), [1]);
    assert_eq!(next_row(&[1]), [1, 1]);
    assert_eq!(next_row(&[1, 1]), [1, 2, 1]);
    assert_eq!(next_row(&[1, 3, 3, 1]), [1, 4, 6, 4, 1]);
    assert_eq!(
        next_row(&next_row(&next_row(&next_row(&next_row(&[]))))),
        [1, 4, 6, 4, 1]
    );

    assert_eq!(
        (0..5).fold(vec![1], |row, _| next_row(&row)),
        [1, 5, 10, 10, 5, 1]
    );

    // Now with scan
    assert_eq!(
        (0..5)
            .scan(vec![], |row, _| {
                *row = next_row(row);
                Some(row.clone())
            })
            .collect::<Vec<_>>(),
        vec![
            vec![1],
            vec![1, 1],
            vec![1, 2, 1],
            vec![1, 3, 3, 1],
            vec![1, 4, 6, 4, 1]
        ]
    );

    // Now with fold again
    assert_eq!(
        (0..5).fold(vec![vec![1]], |mut rows, _| {
            let last_row = rows.last().unwrap();
            rows.push(next_row(last_row));
            rows
        }),
        vec![
            vec![1],
            vec![1, 1],
            vec![1, 2, 1],
            vec![1, 3, 3, 1],
            vec![1, 4, 6, 4, 1],
            vec![1, 5, 10, 10, 5, 1],
        ]
    );

    // now with mutation
    let mut rows = vec![vec![1]];
    for _ in 0..5 {
        rows.push(next_row(&rows.last().unwrap()));
    }
    assert_eq!(
        rows,
        vec![
            vec![1],
            vec![1, 1],
            vec![1, 2, 1],
            vec![1, 3, 3, 1],
            vec![1, 4, 6, 4, 1],
            vec![1, 5, 10, 10, 5, 1],
        ]
    );
}

fn next_row(row: &[u32]) -> Vec<u32> {
    if row.is_empty() {
        return vec![1];
    }
    std::iter::once(1)
        .chain(row.windows(2).map(|win| win.iter().sum()))
        .chain(std::iter::once(1))
        .collect()
}

/*

Conclusion: FP in Rust is kind meh.


*/
fn pascal(n: usize) -> Vec<Vec<u32>> {
    let mut rows = vec![vec![1]];
    for _ in 0..n {
        rows.push(next_row(&rows.last().unwrap()));
    }
    rows
}

fn main() {
    let triangle = pascal(5);
    for row in triangle {
        println!("{:?}", row);
    }
}
