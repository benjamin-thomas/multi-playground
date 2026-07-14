/*

 https://ocaml.org/exercises#12


Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

#  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]


 */

type rle<'a> = One('a) | Many(int, 'a)

let repeat = (n, v) => {
  let rec aux = (acc, m) => m <= 0 ? acc : aux(List.add(acc, v), m - 1)

  aux(list{}, n)
}

let decode = lst =>
  List.reduce(lst, list{}, (acc, x) => {
    switch x {
    | One(v) => List.add(acc, v)
    | Many(n, v) => List.concat(repeat(n, v), acc)
    }
  })->List.reverse

let decode' = lst =>
  List.reduceReverse(lst, list{}, (acc, x) => {
    switch x {
    | One(v) => List.add(acc, v)
    | Many(n, v) => List.concat(repeat(n, v), acc)
    }
  })

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      let got = f =>
        f(
          List.fromArray([
            Many(4, "a"),
            One("b"),
            Many(2, "c"),
            Many(2, "a"),
            One("d"),
            Many(4, "e"),
          ]),
        )->List.toArray
      let want = ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]
      t->expect(want)->Expect.toEqual(got(decode))
      t->expect(want)->Expect.toEqual(got(decode'))
    })
  }
)
