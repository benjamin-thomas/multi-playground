// https://ocaml.org/exercises#1
//
// Write a function `last : 'a list -> 'a option` that returns the last element
// of a list.
//
//   # last ["a" ; "b" ; "c" ; "d"];;
//   - : string option = Some "d"
//
//   # last [];;
//   - : 'a option = None

///|
using @list {type List}

///|
pub fn[A] last(xs : List[A]) -> A? {
  match xs {
    Empty => None
    More(x, tail=Empty) => Some(x)
    More(_, tail=xs) => last(xs)
  }
}

///|
test "impl" {
  let ut = fn(arr) { arr |> @list.from_array() |> last }
  @debug.assert_eq(ut([]), None)
  @debug.assert_eq(ut([1]), Some(1))
  @debug.assert_eq(ut([1, 2, 3]), Some(3))
}
