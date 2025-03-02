///|
pub fn hello() -> String {
  "Hello, world!"
}

///|
fn solution1(cards : Array[Int]) -> Int {
  let m : Map[Int, Int] = {}
  let mut candidate = cards[0]
  for card in cards {
    m[card] = match m[card] {
      None => 1
      Some(n) => n + 1
    }
    match m[candidate] {
      None => ()
      Some(m) => if m > 1 { candidate = card }
    }
  }
  candidate
}

///|
fn solution2(cards : Array[Int]) -> Int {
  let mut result = 0
  for card in cards {
    result = result ^ card
  }
  result
}

///|
fn solution3(cards : Array[Int]) -> Int {
  cards.fold(init=0, fn { acc, n => acc ^ n })
}

///|
fn solution3b(cards : Array[Int]) -> Int {
  cards.fold(init=0, fn(acc, n) { acc ^ n })
}

///|
test "solution3b/basic" {
  inspect!(solution3b([1, 1, 2, 2, 3]), content="3")
  inspect!(solution3b([5, 1, 5, 1, 0]), content="0")
  inspect!(solution3b([7, 2, 7, 3, 2]), content="3")
}

///|
test "solution3b/empty" {
  inspect!(solution3b([]), content="0")
}

///|
test "solution3b/single" {
  inspect!(solution3b([42]), content="42")
}

///|
test "solution3b/empty_array" {
  inspect!(solution3b([]), content="0")
}

///|
test "solution3b/single_element" {
  inspect!(solution3b([99]), content="99")
}

///|
test "solution3b/large_array" {
  let large_test_case = Array::make(10000, 42)
  inspect!(solution3b(large_test_case), content="0")
}

///|
test "hi" {
  if solution1([1, 1, 2, 2, 3, 3, 4, 5, 5]) != 4 {
    fail!("solution 1 failed")
  }
  if solution2([1, 1, 2, 2, 3, 3, 4, 5, 5]) != 4 {
    fail!("solution2 failed")
  }
  if solution3([1, 1, 2, 2, 3, 3, 4, 5, 5]) != 4 {
    fail!("solution3 failed")
  }
}
