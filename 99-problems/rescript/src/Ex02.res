/*

Last Two Elements of a List
Find the last two (last and penultimate) elements of a list.

# last_two ["a"; "b"; "c"; "d"];;
- : (string * string) option = Some ("c", "d")

# last_two ["a"];;
- : (string * string) option = None

 */

let rec last2 = lst => {
  switch lst {
  | list{x, y} => Some(x, y)
  | list{_, ...xs} => last2(xs)
  | _ => None
  }
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("last2", t => {
      t->expect(last2(list{}))->Expect.toEqual(None)
      t->expect(last2(list{"a"}))->Expect.toEqual(None)
      t->expect(last2(list{"a", "b"}))->Expect.toEqual(Some(("a", "b")))
      t->expect(last2(list{"a", "b", "c", "d"}))->Expect.toEqual(Some(("c", "d")))
    })
  }
)
