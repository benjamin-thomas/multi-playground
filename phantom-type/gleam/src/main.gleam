import gleam/io
import password.{Password}
import gleam/string

pub fn main() {
  let pw = password.from_string("p@$$w0rd")

  case password.validate(pw) {
    Ok(Password(pw)) -> {
      io.print(string.concat(["Password check OK: ", pw]))
    }
    Error(_err) -> {
      io.print("Password rejected!")
    }
  }
}
