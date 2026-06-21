/*

https://ocaml.org/exercises#8


Eliminate consecutive duplicates of list elements.

# compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]

 */

let compress = input => {
  let rec aux = (acc, inp) =>
    switch inp {
    | list{} => acc
    | list{x} => acc->List.add(x)
    | list{x, y, ...rest} =>
      x == y ? aux(acc, rest->List.add(y)) : aux(acc->List.add(x), rest->List.add(y))
    }
  aux(list{}, input)->List.reverse
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      let result = compress(
        List.fromArray(["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]),
      )

      t
      ->expect(result->List.toArray)
      ->Expect.toEqual(["a", "b", "c", "a", "d", "e"])
    })
  }
)
