import gleeunit
import gleeunit/should
import lists_.{last, last2, last_alt, nth}
import gleam/option.{None, Some}
import gleam/list

// find ./src/ ./test/ | entr -c gleam test
pub fn main() {
  gleeunit.main()
}

pub fn last_test() {
  last([])
  |> should.equal(None)

  last_alt([1, 2, 3, 4])
  |> should.equal(Some(4))

  last([1])
  |> should.equal(Some(1))

  // Fails with a timeout error if I remove the division
  // Is this equivalent to a stack overflow? I'm using `list.fold` here.
  // > %% Unknown error: {timeout...
  let too_big = 99_999_999 / 10_000_000
  last_alt(list.range(1, too_big))
  |> should.equal(Some(9))
}

pub fn last2_test() {
  last2([])
  |> should.equal(None)

  last2([1])
  |> should.equal(None)

  last2([1, 2])
  |> should.equal(Some(#(1, 2)))

  last2([1, 2, 3])
  |> should.equal(Some(#(2, 3)))
}

pub fn nth_test() {
  [1, 2, 3]
  |> nth(0)
  |> should.equal(Some(1))

  [1, 2, 3]
  |> nth(1)
  |> should.equal(Some(2))

  [1, 2, 3]
  |> nth(2)
  |> should.equal(Some(3))

  [1, 2, 3]
  |> nth(3)
  |> should.equal(None)

  [1, 2, 3]
  |> nth(-1)
  |> should.equal(None)
}
