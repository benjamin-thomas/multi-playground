/*
echo monads.v | entr -c v run /_
*/

// Optional arguments or not supported, yet.
fn add(x int, y int) int {
	return x + y
}

fn user_input(s string) ?int {
	n := s.int()
	if n.str() == s {
		return n
	} else {
		return none
	}
}

[noreturn]
fn die(msg string) {
	println(msg)
	exit(1)
}

/*
The monad pattern doesn't seem really possible with vlang

Instead of:
	val1 -> then
		val 2 -> then
			val1 + val2

I need to validate data at the top of the computation, then feed non-optional data to later functions.
*/
fn main() {
	a := user_input('1') or { die('bad input for a') }
	b := user_input('2') or { die('bad input for b') }
	res := add(a, b)
	println('1 + 1 = $res')

	/*
	The verbose syntax below is not valid, `and` does not exist.
	So it is not possible to consider going further and "de-indent" those would-be function calls.
	a := user_input('1') and {
		b := user_input('2') and {
			res := add(a, b)
			println('1 + 1 = $res')
		}
	}
	*/
}
