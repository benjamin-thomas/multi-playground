// clang-format off

// echo ./solution.c | entr -c bash -c 'gcc -o /tmp/solution ./solution.c && /tmp/solution && echo OK'

// clang-format on

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

void assertEqual(char *a, char *b) {
  if (strcmp(a, b) != 0) {
    printf("%s != %s\n", a, b);
    exit(1);
  }
}

char *mergeAlternately(const char *word1, const char *word2) {
  int len_a = strlen(word1);
  int len_b = strlen(word2);

  char *result = (char *)malloc(sizeof(char) * (len_a + len_b + 1));
  if (result == NULL) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }
  // TODO: free result!

  int i = 0;
  int j = 0;
  while (i < len_a || i < len_b) {
    if (i < len_a)
      result[j++] = word1[i];

    if (i < len_b)
      result[j++] = word2[i];

    i++;
  }
  result[j] = '\0';

  return result;
}

int main() {
  assertEqual(mergeAlternately("ABC", "abc"), "AaBbCc");
  assertEqual(mergeAlternately("AB", "ab_cde"), "AaBb_cde");
  assertEqual(mergeAlternately("AB_CDE", "ab"), "AaBb_CDE");
  assertEqual(mergeAlternately("", "abc"), "abc");
  assertEqual(mergeAlternately("ABC", ""), "ABC");
  assertEqual(mergeAlternately("", ""), "");
  return 0;
}