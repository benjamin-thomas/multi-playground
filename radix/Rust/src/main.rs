#[derive(Debug)]
enum AppError {
    ParseError(#[allow(dead_code)] char),
    IoError(#[allow(dead_code)] std::io::Error),
}

fn digit_of_char(c: char) -> Result<u8, AppError> {
    match c {
        '0'..='9' => Ok((c as u8) - b'0'),
        'A'..='Z' => Ok((c as u8) - b'A' + 10),
        'a'..='z' => Ok((c as u8) - b'a' + 10),
        _ => Err(AppError::ParseError(c)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digit_of_char() {
        assert!(matches!(digit_of_char('$'), Err(AppError::ParseError('$'))));
        assert!(matches!(digit_of_char('0'), Ok(0)));
        assert!(matches!(digit_of_char('F'), Ok(15)));
        assert!(matches!(digit_of_char('f'), Ok(15)));
    }
}

fn hex_to_digits(digits: &[u8]) -> usize {
    digits.iter().fold(0, |acc, &x| (x as usize) + acc * 16)
}

fn run_loop() -> Result<(), AppError> {
    let stdin = std::io::stdin();
    let mut handle = stdin.lock();
    let mut buf = String::with_capacity(1024);

    loop {
        buf.clear();

        match std::io::BufRead::read_line(&mut handle, &mut buf).map_err(AppError::IoError)? {
            0 => break, // EOF
            _ => {
                let input = buf.trim();
                println!("{}", input);

                let digits = input
                    .chars()
                    .map(digit_of_char)
                    .collect::<Result<Vec<u8>, AppError>>()?;

                let result = hex_to_digits(&digits);

                println!(" -> {:}\n", result)
            }
        }
    }
    Ok(())
}

/*

rg --files | entr -c cargo test
rg --files | entr -rc bash -c 'cargo run < <(echo -e "123\nFF")'

*/
fn main() {
    println!("Starting up...");
    match run_loop() {
        Err(x) => eprintln!("Program failed: {:?}", x),
        Ok(()) => {}
    }

    println!("Finished!");
}
