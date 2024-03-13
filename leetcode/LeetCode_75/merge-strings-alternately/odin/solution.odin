package main

// odin run ./solution.odin -file
// echo ./solution.odin | entr -c odin test /_ -file

import "core:fmt"
import "core:strings"
import "core:testing"

add :: proc(x: int, y: int) -> int {
	return x + y
}

@(test)
test_add :: proc(t: ^testing.T) {
	testing.expect(t, true == true)
	testing.expect_value(t, add(1, 2), 3)
}

join :: proc(x: string, y: string) -> string {
	return strings.concatenate([]string{x, y})
}

@(test)
test_join :: proc(t: ^testing.T) {
	testing.expect_value(t, join("a", "b"), "ab")
}

interleave_letters :: proc(word_a: string, word_b: string) -> string {
	chars_a := strings.split(word_a, "")
	defer delete(chars_a)

	chars_b := strings.split(word_b, "")
	defer delete(chars_b)

	sb: strings.Builder
	strings.builder_init(&sb, (len(word_a) + len(word_b)))

	for i := 0; i < len(chars_a) || i < len(chars_b); i += 1 {
		if i < len(chars_a) {
			strings.write_string(&sb, chars_a[i])
		}
		if i < len(chars_b) {
			strings.write_string(&sb, chars_b[i])
		}
	}
	return strings.to_string(sb)
}

@(test)
test_interleave_letters :: proc(t: ^testing.T) {
	/*
    I'm not sure what's wrong:
      expected ABCD, got ABCD
    */
	testing.expect_value(t, interleave_letters("AC", "BD"), "ABCD")
	testing.expect(t, strings.compare(interleave_letters("AC", "BD"), "ABCD") == 0)
	// testing.expect_value(t, interleave_letters("AC_EF", "BD"), "ABCD_EF")
}

main :: proc() {
	fmt.println("Hello, world!")
}
