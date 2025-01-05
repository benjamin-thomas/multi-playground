#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #include "./utils.h"

/*

Terminal 1:
  echo ./day01.c | entr -c bash -c 'clang-tidy ./day01.c && cmake --build ./build'
  # Or
  rg --files -t c | entr -c bash -c 'clang-tidy ./day01.c && cmake --build ./build'

Terminal 2:
  echo build/day01 | entr -c /_
  # or just reload via gdb
*/

int compare_int(const void* a, const void* b) { return *(int*)a - *(int*)b; }

#define seen_max_val 0xFF << 9

void solve(FILE* file) {
  int seen[seen_max_val] = {0};
  char line[32];
  int capacity = 0xFF;

  int* xs = malloc(capacity * sizeof(int));
  int* ys = malloc(capacity * sizeof(int));
  assert(xs && ys);

  int distances = 0;
  int similarities = 0;
  {
    int size = 0;
    while (fgets(line, sizeof(line), file)) {
      // Grow array capacity if necessary
      if (size >= capacity) {
        capacity = capacity << 1;
        xs = realloc(xs, capacity * sizeof(int));
        ys = realloc(ys, capacity * sizeof(int));
        assert(xs && ys);
      }

      // Parse left/right numbers
      int left, right;
      char* token = strtok(line, " ");
      left = atoi(token);
      token = strtok(NULL, " ");
      right = atoi(token);

      assert(right >= 0);  // sanity check
      assert(right <= seen_max_val &&
             "Add one more bit shift if this is triggered");

      // Mark right number seen
      seen[right]++;

      xs[size] = left;
      ys[size] = right;
      size++;  // track the actual size as we go, need it later.
    }

    qsort(xs, size, sizeof(int), compare_int);
    qsort(ys, size, sizeof(int), compare_int);

    for (int i = 0; i < size; i++) {
      distances += abs(ys[i] - xs[i]);
    }

    for (int i = 0; i < size; i++) {
      similarities += xs[i] * seen[xs[i]];
    }
  }

  // %12 left-pads with up to 12 spaces
  printf("Part 1 (total distances):    %12d\t(OK=%d)\n", distances,
         distances == 1189304);
  printf("Part 2 (total similarities): %12d\t(OK=%d)\n", similarities,
         similarities == 24349736);

  free(ys);
  free(xs);
}

int main(void) {
  const char* filename = "../_inputs/01.txt";
  FILE* file = fopen(filename, "r");
  if (!file) {
    printf("Failed to open file: %s\n", filename);
    exit(1);
  }

  solve(file);

  fclose(file);
  return 0;
}
