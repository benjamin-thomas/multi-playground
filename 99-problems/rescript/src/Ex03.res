/*

N'th Element of a List
Find the N'th element of a list.

# at 2 ["a"; "b"; "c"; "d"; "e"];;
- : string option = Some "c"

# at 2 ["a"];;
- : string option = None

 */

let rec at = (lst, n) => {
  switch (lst, n) {
  | (list{}, _) => None
  | (list{x, ..._}, 0) => Some(x)
  | (list{_, ...rest}, n) => at(rest, n - 1)
  }
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      t->expect(list{"a", "b", "c", "d"}->at(2))->Expect.toEqual(Some("c"))
      t->expect(list{"a"}->at(2))->Expect.toEqual(None)
    })
  }
)
