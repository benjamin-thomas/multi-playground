// rg --files -t rust | entr -c cargo run

/*

ARGON2_PUBLIC int argon2id_verify
( const   char *encoded
, const   void *pwd
, const size_t  pwdlen
);

*/

use std::ffi::c_char;
use std::ffi::CString;

#[link(name = "argon2")]
extern "C" {
    fn argon2id_verify(hash: *const c_char, pwd: *const c_char, pwdlen: usize) -> i32;
}

const HASH: &str = "$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw";

fn verify(pwd: &str) -> bool {
    let hash = CString::new(HASH).unwrap();
    let pwd = CString::new(pwd).unwrap();
    let pwdlen = pwd.as_bytes().len();

    0 == unsafe { argon2id_verify(hash.as_ptr(), pwd.as_ptr(), pwdlen) }
}

fn main() {
    let pwd = std::env::args().nth(1).expect("Usage: ./main <password>");

    if verify(&pwd) {
        println!("Password is correct");
    } else {
        println!("Password is not correct");
    }
}
