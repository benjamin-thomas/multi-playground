open Vitest.Benchmark

let fewLargeRuns = list{
  Ex12.Many(10_000, "a"),
  Ex12.Many(10_000, "b"),
  Ex12.Many(10_000, "c"),
  Ex12.Many(10_000, "d"),
  Ex12.Many(10_000, "e"),
}

describe("Ex12.decode", () => {
  bench("reduce + reverse", _ => {
    Ex12.decode(fewLargeRuns)->ignore
  })

  bench("reduceReverse", _ => {
    Ex12.decode'(fewLargeRuns)->ignore
  })
})
