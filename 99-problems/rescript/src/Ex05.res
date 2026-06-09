/*

 https://ocaml.org/exercises#5


Reverse a List

OCaml standard library has List.rev but we ask that you reimplement it.

# rev ["a"; "b"; "c"];;
- : string list = ["c"; "b"; "a"]

 */

let rev = lst => {
  let rec aux = (acc, lst) => {
    switch lst {
    | list{} => acc
    | list{x, ...rest} => aux(List.add(acc, x), rest)
    }
  }
  aux(list{}, lst)
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      t->expect(rev(list{"a", "b", "c"}))->Expect.toEqual(list{"c", "b", "a"})
    })
  }
)
