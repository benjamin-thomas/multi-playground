/*

 https://ocaml.org/exercises#11


Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list.
Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

type 'a rle =
  | One of 'a
  | Many of int * 'a

# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

 */

// Run Length Encoding output
type rle<'a> = One('a) | Many(int, 'a)

let encode = lst => {
  let rec aux = (prev, acc1, acc2, lst) => {
    switch lst {
    | list{} => List.reverse(List.add(acc2, acc1))
    | list{x, ...xs} =>
      if prev == x {
        switch acc1 {
        | One(_) => aux(x, Many(2, x), acc2, xs)
        | Many(n, _) => aux(x, Many(n + 1, x), acc2, xs)
        }
      } else {
        aux(x, One(x), List.add(acc2, acc1), xs)
      }
    }
  }

  switch lst {
  | list{} => list{}
  | list{x, ...xs} => aux(x, One(x), list{}, xs)
  }
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    let got =
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
      ->encode
      ->List.toArray

    let want =
      list{Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("d"), Many(4, "e")}->List.toArray

    test("impl", t => {
      t->expect(got)->Expect.toEqual(want)
    })
  }
)
