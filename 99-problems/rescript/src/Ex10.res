/*

https://ocaml.org/exercises#10

If you need to, refresh your memory about run-length encoding: https://en.wikipedia.org/wiki/Run-length_encoding

Here is an example:

# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

 */

let encode = lst => {
  let rec aux = (acc, curr, lst) => {
    switch lst {
    | list{} => List.reverse(List.add(acc, curr))
    | list{x, ...rest} =>
      let (n, prev) = curr
      if x == prev {
        aux(acc, (n + 1, x), rest)
      } else {
        aux(List.add(acc, curr), (1, x), rest)
      }
    }
  }
  switch lst {
  | list{} => list{}
  | list{x, ...rest} => aux(list{}, (1, x), rest)
  }
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    let got =
      list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"}
      ->encode
      ->List.toArray

    let want = list{(4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e")}->List.toArray

    test("impl", t => {
      t->expect(got)->Expect.toEqual(want)
    })
  }
)
