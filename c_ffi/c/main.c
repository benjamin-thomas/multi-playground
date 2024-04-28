/*
compile and run:
  gcc main.c -o /tmp/tmp -l argon2 && /tmp/tmp hello

*/

#include <argon2.h>
#include <stdio.h>
#include <string.h>

const char *HASHED =
    "$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw";

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: %s <password>\n", argv[0]);
    return 1;
  }
  char *pw = argv[1];
  size_t pw_len = strlen(pw);
  int result = argon2id_verify(HASHED, pw, pw_len);

  if (result == ARGON2_OK) {
    printf("Password is correct\n");
  } else if (result == ARGON2_VERIFY_MISMATCH) {
    printf("Password is incorrect\n");
  } else {
    printf("Unexpected error: %d\n", result);
  }
}