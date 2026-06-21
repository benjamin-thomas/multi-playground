/*

https://ocaml.org/exercises#9

Pack consecutive duplicates of list elements into sublists.

# pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]


 */

let pack = lst => {
  let rec aux = (acc1, acc2, x, xs) => {
    switch xs {
    | list{} => List.reverse(List.add(acc2, acc1))
    | list{y, ...rest} =>
      x == y ? aux(List.add(acc1, x), acc2, y, rest) : aux(list{y}, List.add(acc2, acc1), y, rest)
    }
  }

  switch lst {
  | list{} => list{}
  | list{x, ...xs} => aux(list{x}, list{}, x, xs)
  }
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      let toArray = xss => xss->List.toArray->Array.map(List.toArray)

      let got =
        list{"a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "d", "e", "e", "e", "e"}
        ->pack
        ->toArray

      let want =
        list{
          list{"a", "a", "a", "a"},
          list{"b"},
          list{"c", "c"},
          list{"a", "a"},
          list{"d", "d"},
          list{"e", "e", "e", "e"},
        }->toArray

      t
      ->expect(got)
      ->Expect.toEqual(want)
    })
  }
)
