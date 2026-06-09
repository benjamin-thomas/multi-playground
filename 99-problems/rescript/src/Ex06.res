/*

 https://ocaml.org/exercises#6

 Find out whether a list is a palindrome.

Hint: A palindrome is its own reverse.

# is_palindrome ["x"; "a"; "m"; "a"; "x"];;
- : bool = true
# not (is_palindrome ["a"; "b"]);;
- : bool = true


 */

let isPalindrome = lst => {
  lst == List.reverse(lst)
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      t->expect(isPalindrome(list{"x", "a", "m", "a", "x"}))->Expect.toBe(true)
      t->expect(isPalindrome(list{"a", "b"}))->Expect.toBe(false)
    })
  }
)
