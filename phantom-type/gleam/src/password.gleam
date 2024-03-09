import gleam/string

pub type Unverified

pub type Valid

pub type Password(validation) {
  Password(String)
}

pub type Reason {
  InsufficientLength
}

pub fn from_string(str) -> Password(Unverified) {
  Password(str)
}

pub fn validate(pw: Password(Unverified)) -> Result(Password(Valid), Reason) {
  case pw {
    Password(p) -> {
      case string.length(p) > 0 {
        True -> Ok(Password(p))
        False -> Error(InsufficientLength)
      }
    }
  }
}
