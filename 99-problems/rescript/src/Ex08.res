/*

https://ocaml.org/exercises#8


Eliminate consecutive duplicates of list elements.

# compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]

 */

let compress = input => {
  let rec aux = (acc, x, inp) =>
    switch inp {
    | list{} => List.reverse(acc)
    | list{y, ...rest} => x == y ? aux(acc, y, rest) : aux(List.add(acc, y), y, rest)
    }

  switch input {
  | list{} => list{}
  | list{x, ...rest} => aux(list{x}, x, rest)
  }
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
