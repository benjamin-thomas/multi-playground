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

let flatten: list<node<'a>> => list<'a> = nodes => {
  let rec aux = (acc, nodes) => {
    switch nodes {
    | list{} => acc
    | list{One(item), ...rest} => aux(List.add(acc, item), rest)
    | list{Many(items), ...rest} => aux(acc, List.concat(items, rest))
    }
  }
  List.reverse(aux(list{}, nodes))
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
