import gleam/string

fn interleave(chars_a: List(String), chars_b: List(String)) -> String {
  case #(chars_a, chars_b) {
    #([], []) -> ""
    #([x, ..xs], []) -> x <> interleave(xs, [])
    #([], [x, ..ys]) -> x <> interleave([], ys)
    #([x, ..xs], [y, ..ys]) -> x <> y <> interleave(xs, ys)
  }
}

pub fn interleave_letters(word_a: String, word_b: String) -> String {
  interleave(string.to_graphemes(word_a), string.to_graphemes(word_b))
}
