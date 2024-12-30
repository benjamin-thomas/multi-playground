#include <stdio.h>
#include <stdlib.h>

/*

gcc -g -Wall -Wextra -pedantic day01.c && valgrind a.out
rg --files | entr -rc bash -c 'gcc -g -Wall -Wextra -pedantic day01.c -o /tmp/tmp && valgrind /tmp/tmp'

*/

void work(void) {
  int arr[5];
  arr[4] = 10;  // Invalid access2

  printf("arr len is: %lu\n", sizeof(arr));

  char *ptr = malloc(10);
  if (!ptr) {
    printf("Failed to allocate memory\n");
    return;
  }
  free(ptr);
  printf("wat");
}

int main(void) {
  int i = 0;

  while (i < 10) {
    i++;
    printf("hello2\n");
  }

  work();
}
