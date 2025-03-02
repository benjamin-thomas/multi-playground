// https://oj.moonbitlang.com/problems/202412-001-find-the-unique-number

///|
pub fn solution(cards : Array[Int]) -> Int {
  cards.fold(init=0, fn(acc, n) { acc ^ n })
}

///|
test {
  inspect!(solution([1, 1, 2, 2, 3, 3, 4, 5, 5]), content="4")
  inspect!(solution([0, 1, 0, 1, 2]), content="2")
  inspect!(solution([7, 3, 3, 7, 10]), content="10")
}
