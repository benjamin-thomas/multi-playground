import gleam/option.{None, Some}
import gleam/list

pub fn last(lst) {
  case lst {
    [] -> None
    [x] -> Some(x)
    [_, ..xs] -> last(xs)
  }
}

pub fn last2(lst) {
  lst
  |> list.fold(None, fn(_acc, x) { Some(x) })
}
