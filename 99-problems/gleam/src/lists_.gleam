import gleam/option.{type Option, None, Some}
import gleam/list

// Exercises at: https://ocaml.org/exercises#1

// 1) Find the last item of a list
pub fn last(lst) {
  case lst {
    [] -> None
    [x] -> Some(x)
    [_, ..xs] -> last(xs)
  }
}

// 1) Find the last item of a list (alternative)
pub fn last_alt(lst) {
  lst
  |> list.fold(None, fn(_acc, x) { Some(x) })
}

// 2) Find the last but one (last and penultimate) elements of a list.
// pub fn
pub fn last2(lst) {
  case lst {
    [] -> None
    [x, y] -> Some(#(x, y))
    [_, ..rest] -> last2(rest)
  }
}

// 3) Find the N'th element of a list.
pub fn nth(lst: List(a), n: Int) -> Option(a) {
  case lst {
    [] -> None
    [x, ..xs] ->
      case n {
        0 -> Some(x)
        _ -> nth(xs, n - 1)
      }
  }
}
