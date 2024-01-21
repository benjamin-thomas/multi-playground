import gleeunit
import gleeunit/should
import lists_
import gleam/option.{None, Some}
import gleam/list

// find ./src/ ./test/ | entr -c gleam test
pub fn main() {
  gleeunit.main()
}

pub fn last_test() {
  lists_.last([])
  |> should.equal(None)

  lists_.last2([1, 2, 3, 4])
  |> should.equal(Some(4))

  lists_.last([1])
  |> should.equal(Some(1))

  // Fails with a timeout error if I remove the division
  // Is this equivalent to a stack overflow? I'm using `list.fold` here.
  // > %% Unknown error: {timeout...
  let too_big = 99_999_999 / 10_000_000
  lists_.last2(list.range(1, too_big))
  |> should.equal(Some(9))
}
