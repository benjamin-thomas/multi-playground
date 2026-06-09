/*

 https://ocaml.org/exercises#0


 */

%%private(
  if Vitest.inSource {
    open Vitest.InSource

    test("impl", t => {
      t->expect(1 + 2)->Expect.toEqual(3)
    })
  }
)
