// Write a function last : 'a list -> 'a option that returns the last element of a list

/*

 # last ["a" ; "b" ; "c" ; "d"];;
- : string option = Some "d"

# last [];;
- : 'a option = None

 */

let rec last = lst => {
  switch lst {
  | list{} => None
  | list{x} => Some(x)
  | list{_, ...xs} => last(xs)
  }
}

let last' = (arr: array<'a>) => {
  // Different ways to do the same thing...
  let impls = [
    () => arr[arr->Array.length - 1],
    () => arr->Array.get(arr->Array.length - 1),
    () => arr->Array.last,
  ]
  impls->Array.shuffle
  let candidate = impls->Array.getUnsafe(0)
  candidate()
}

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("last returns the last list element", t => {
      t->expect(last(list{1, 2, 3}))->Expect.toEqual(Some(3))
    })

    test("last returns None for an empty list", t => {
      t->expect(last(list{}))->Expect.toEqual(None)
    })

    test("last' returns the last array item", t => {
      t->expect(last'([1, 2, 3]))->Expect.toEqual(Some(3))
    })

    test("last' returns None when the array is empty", t => {
      t->expect(last'([]))->Expect.toEqual(None)
    })
  }
)
