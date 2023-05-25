import gleeunit
import gleeunit/should
import monads
import gleam/option.{None, Some}

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn calls_external_functions_test() {
  monads.hello()
  |> should.equal("world")
}

pub fn inc_test() {
  monads.inc(None)
  |> should.equal(None)

  monads.inc(Some(1))
  |> should.equal(Some(2))
}

pub fn add_test() {
  monads.add(Some(1), Some(2))
  |> should.equal(Some(3))
}

pub fn mul_test() {
  monads.mul(Some(3), Some(4))
  |> should.equal(Some(12))

  monads.mul(None, Some(4))
  |> should.equal(None)

  monads.mul(Some(4), None)
  |> should.equal(None)
}

pub fn min_test() {
  monads.min(Ok(10), Ok(2), Ok(3))
  |> should.equal(Ok(5))

  // Fails on the first error OK
  monads.min(Ok(10), Error("@2"), Error("@3"))
  |> should.equal(Error("@2"))
}
