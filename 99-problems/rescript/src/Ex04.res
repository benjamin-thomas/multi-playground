/*

 https://ocaml.org/exercises#4

 Length of a List
 Find the number of elements of a list.

 OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.

 # length ["a"; "b"; "c"];;
 - : int = 3
 # length [];;
 - : int = 0

 */

let length = lst => {
  let rec aux = (n, lst) =>
    switch lst {
    | list{} => n
    | list{_, ...rest} => aux(n + 1, rest)
    }
  aux(0, lst)
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      t->expect(length(list{1, 2, 3}))->Expect.toEqual(3)
      t->expect(length(list{}))->Expect.toEqual(0)
    })
  }
)
