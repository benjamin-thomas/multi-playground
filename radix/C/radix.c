#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

/*

clang-format off

rg --files | entr -c bash -c 'gcc -Wall -Wpedantic -o /tmp/tmp radix.c && echo OK'
rg --files | entr -c bash -c 'gcc -Wall -Wpedantic -o /tmp/tmp radix.c && /tmp/tmp 16 < <(echo -e "ABC\nDEF\nFF")'

clang-format on
*/

#define RED "\033[1;31m"
#define RESET "\033[0m"

void __attribute__((noreturn)) die(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  exit(EXIT_FAILURE);
}

int digit_of_char(char c) {
  if (c >= '0' && c <= '9') {
    return c - '0';
  } else if (c >= 'A' && c <= 'Z') {
    return c - 'A' + 10;
  } else if (c >= 'a' && c <= 'z') {
    return c - 'a' + 10;
  }

  die("Failure, digit_of_char: '%c' (%d)\n", c, c);
}

int require_int(int argc, char *argv[], int idx) {
  if (argc <= idx)
    die("Usage: %s BASE\n", argv[0]);

  char *endptr;
  errno = 0;
  int v = strtol(argv[idx], &endptr, 10);

  if (errno == ERANGE) {
    die("Numeric value out of range: %s\n", argv[idx]);
  }

  if (endptr == argv[1]) {
    die("Not a numeric value: %s\n", argv[idx]);
  }

  if (*endptr != '\0') {
    die("Extra characters after number: %s\n", endptr);
  }

  return v;
}

int convert(int base, const char *str, int *result) {
  int acc = 0;
  for (int i = 0; str[i] != '\n' && str[i] != '\0'; i++) {
    printf("%d ", str[i]);

    int val = digit_of_char(str[i]);
    if (val < 0 || val >= base) {
      return -1;
    }

    acc = val + base * acc;
  }
  *result = acc;
  return 0;
}

#define MAX_LEN 10
void run_program(int base) {
  int len = MAX_LEN;
  char line[len];
  while (fgets(line, sizeof(line), stdin)) {
    printf("\n%s", line);
    int result;
    if (convert(base, line, &result) != 0) {
      printf(RED);
      printf("-- BAD DATA, skipping line %s\n", line);
      printf(RESET);
      printf("\033[1A"); // cursor up
      continue;
    }
    printf("-> %d\n", result);
  }
}

int main(int argc, char *argv[]) {
  int base = require_int(argc, argv, 1);
  printf("Base: %d\n", base);
  printf("---\n");
  run_program(base);
  return 0;
}
