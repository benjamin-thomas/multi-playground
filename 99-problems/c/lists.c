#include <stdio.h>

/*
rg --files | entr -c bash -c './lint.sh && ./run.sh'
*/

/*
P01 (*) Find the last element of a list.
*/
// The impls for P01 will implement this interface
typedef int (*P01)(int[], size_t);

// Here, we actually pass `arr` as a pointer
// `static` makes the function private to this file (un-exported)
static int lastA(int arr[], size_t size) {
  arr[0] = 99; // for demonstration purposes
  return arr[size - 1];
}

// In this version, we make it clearer that `arr` is in fact a pointer
static int lastB(int *arr, size_t size) {
  arr[0] = 98; // for demonstration purposes
  return arr[size - 1];
}

static void test_last(P01 fn) {
  int arr[] = {1, 2, 3, 4, 5};
  printf("First element is: %d\n", arr[0]);

  /*
  sizeof(arr) returns the total size in bytes of the array arr.

  sizeof(arr[0]) returns the size in bytes of a single element
  in the array. Dividing the total size of the array by the size
  of a single element gives you the number of elements in the array.

  size_t is a data type in C that's used to represent the size of
  objects in memory.
  It's an unsigned integer type and is typically returned by the
  sizeof operator and various memory-related functions, such as
  strlen() and malloc().
  */
  size_t arrSize = sizeof(arr) / sizeof(arr[0]);
  int l = fn(arr, arrSize);
  printf("Last element is: %d\n", l);
  printf("First element now is: %d\n", arr[0]);
}

int main() {
  printf("== Testing P01 (A)\n");
  test_last(lastA);
  printf("== Testing P01 (B)\n");
  test_last(lastB);

  return 0;
}
