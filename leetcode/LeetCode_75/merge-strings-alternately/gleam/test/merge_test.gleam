import gleeunit
import gleeunit/should
import merge

// find -name '*.gleam' | entr -c gleam test

pub fn main() {
  gleeunit.main()
}

pub fn interleave_letters_test() {
  merge.interleave_letters("AC", "BD")
  |> should.equal("ABCD")

  merge.interleave_letters("ACEFG", "BD")
  |> should.equal("ABCDEFG")

  merge.interleave_letters("AC", "BDEFG")
  |> should.equal("ABCDEFG")

  merge.interleave_letters("ABC", "")
  |> should.equal("ABC")

  merge.interleave_letters("", "ABC")
  |> should.equal("ABC")

  merge.interleave_letters("", "")
  |> should.equal("")
}
