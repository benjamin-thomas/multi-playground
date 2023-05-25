import gleam/io
import gleam/option.{Option, Some, then}
import gleam/result

// find src/ test/ | entr -cr gleam test

pub fn hello() -> String {
  "world"
}

pub fn inc(a: Option(Int)) -> Option(Int) {
  a
  |> then(fn(x) { Some(x + 1) })
}

// "Normal" mode indents!
pub fn add(a: Option(Int), b: Option(Int)) -> Option(Int) {
  a
  |> then(fn(x) {
    b
    |> then(fn(y) { Some(x + y) })
  })
}

// This technique avoids nesting
pub fn mul(a: Option(Int), b: Option(Int)) -> Option(Int) {
  use x <- then(a)
  use y <- then(b)
  Some(x * y)
}

pub fn min(
  a: Result(Int, e),
  b: Result(Int, e),
  c: Result(Int, e),
) -> Result(Int, e) {
  use x <- result.then(a)
  use y <- result.then(b)
  use z <- result.then(c)
  Ok(x - y - z)
}

pub fn main() {
  io.println("Hello from monads!")
}
