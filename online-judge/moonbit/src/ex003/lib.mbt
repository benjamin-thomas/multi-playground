// https://oj.moonbitlang.com/problems/202412-003-number-string-formatting

///|
fn removeLoadingZeros(str : String) -> String {
  let arr : Array[Char] = []
  let mut stripping = true
  for c in str {
    if c == '0' && stripping {
      continue
    }
    stripping = false
    arr.push(c)
  }
  String::from_array(arr)
}

///|
test "leading zeros" {
  inspect!(removeLoadingZeros("00001234"), content="1234")
  inspect!(removeLoadingZeros("0100"), content="100")
}

///|
fn solution(str : String) -> String {
  let chars : Array[Char] = []
  let (int, dec) = match removeLoadingZeros(str).split(".").to_array() {
    [int, dec] => (int, "." + dec)
    [int] => (int, "")
    _ => ("", "") // should fail really, but I can't modify the "solution" signature
  }
  int
  .rev_iter()
  .eachi(fn(i, c) {
    if i > 0 && i % 3 == 0 {
      chars.push(',')
    }
    chars.push(c)
  })
  String::from_array(chars.rev()) + dec
}

///|
test "solution" {
  inspect!(solution(""), content="")
  inspect!(solution("123"), content="123")
  inspect!(solution("1234"), content="1,234")
  inspect!(solution("12345"), content="12,345")
  inspect!(solution("123456"), content="123,456")
  inspect!(solution("1234567"), content="1,234,567")
  inspect!(solution("0001234567"), content="1,234,567")
  inspect!(solution("1234.9"), content="1,234.9")
  inspect!(solution("123456.98765"), content="123,456.98765")
  inspect!(solution("1234567.987654321"), content="1,234,567.987654321")
}
