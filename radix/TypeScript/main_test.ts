import { assertEquals } from "@std/assert";
import { toBase10 } from "./main.ts";

// deno test --watch
Deno.test(function toBase10Test() {
  assertEquals(toBase10("5"), 5);
  assertEquals(toBase10("a"), 10);
  assertEquals(toBase10("F"), 15);
});
