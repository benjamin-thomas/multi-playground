/*

 https://ocaml.org/exercises#7


Flatten a nested list structure.

type 'a node =
  | One of 'a
  | Many of 'a node list

# flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
- : string list = ["a"; "b"; "c"; "d"; "e"]

 */

type rec node<'a> =
  | One('a)
  | Many(list<node<'a>>)

let rec flatten: list<node<'a>> => list<'a> = nodes => {
  let rec aux: (list<'a>, list<node<'a>>) => list<'a> = (acc: list<'a>, nodes) => {
    switch nodes {
    | list{} => acc
    | list{x, ...rest} =>
      switch x {
      | One(item) => aux(List.add(acc, item), rest)
      | Many(items) => {
          let next = flatten(items)
          let acc = List.concat(next, acc)
          aux(acc, rest)
        }
      }
    }
  }
  aux(list{}, List.reverse(nodes))
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      let test' = (~want, ~got, ~skip=false) => {
        if skip {
          ()
        } else {
          t->expect(got->List.toArray)->Expect.toEqual(want)
        }
      }

      test'(~want=[], ~got=flatten(list{}))
      test'(~want=["a"], ~got=flatten(list{One("a")}))
      test'(
        ~skip=false,
        ~want=["a", "b"],
        ~got=flatten(list{
          //
          Many(list{One("a"), One("b")}),
        }),
      )
      test'(
        ~skip=false,
        ~want=["a", "b", "c", "d", "e"],
        ~got=flatten(list{
          //
          One("a"),
          Many(list{
            //
            One("b"),
            Many(list{One("c"), One("d")}),
            One("e"),
          }),
        }),
      )

      test'(
        ~skip=false,
        ~want=["a", "b", "c"],
        ~got=flatten(list{
          One("a"),
          One("b"),
          //
          Many(list{One("c")}),
        }),
      )
    })
  }
)
