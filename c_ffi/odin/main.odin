package main

import "core:c"
import "core:fmt"
import "core:os"
import "core:strings"

/*
echo ./main.odin | entr -c odin run /_ -file
echo ./main.odin | entr -c odin run /_ -file -- hello

odin build ./main.odin -file
*/

// /usr/lib/x86_64-linux-gnu/libargon2.so.1
when ODIN_OS == .Linux do foreign import argon2 "system:argon2"

foreign argon2 {
	argon2id_verify :: proc(hash: cstring, pwd: cstring, pwlen: c.uint) -> c.int ---
}

HASH :: cstring("$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw")

main :: proc() {
	// pwd :: cstring("hello")
	pwd1 := os.args[1]
	pwd: cstring = strings.clone_to_cstring(pwd1)
	pwd_len: u32 = u32(len(pwd))
	fmt.printfln("Password length is: %d", pwd_len)
	result := argon2id_verify(HASH, pwd, pwd_len)

	if result == 0 {
		fmt.println("Password is valid")
	} else {
		fmt.println("Password is invalid")
	}
	fmt.printfln("Result was: %d", result)
}
