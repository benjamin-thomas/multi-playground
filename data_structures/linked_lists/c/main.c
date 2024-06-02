#include <stdio.h>

/*
clang-format off
Compile with (pedantic mode):

gcc -Wall -Wpedantic -Wextra -Wdeclaration-after-statement -g -o /tmp/tmp main.c
echo ./main.c | entr -c bash -c 'gcc -Wall -Wpedantic -Wextra -Wdeclaration-after-statement -g -o /tmp/tmp main.c && echo "$(date -Iseconds): OK"'
echo ./main.c | entr -c bash -c 'gcc -Wall -Wpedantic -Wextra -Wdeclaration-after-statement -g -o /tmp/tmp main.c && /tmp/tmp'

clang-format on
*/

// typedef struct list {
//   int val;
//   struct list *next;
// } list;

struct node {
  int val;
  struct node *next;
};

typedef struct node list;

struct node *init_list(struct node nodes[]) {
  for (int i = 0; nodes[i].next != NULL; i++) {
    nodes[i].next = &nodes[i + 1];
  }
  return nodes;
}

void print_list(struct node *lst) {
  while (lst != NULL) {
    printf("%d\n", lst->val);
    lst = lst->next;
  }
}

int main() {
  struct node c = {.val = 3, .next = NULL};
  struct node b = {.val = 2, .next = &c};
  struct node a = {.val = 1, .next = &b};
  printf("AAA\n");
  print_list(&a);

  // clang-format off
  struct node lst =
    { 1, &(list)
    { 2, &(list)
    { 3, &(list)
    { 4, &(list)
    { 5, &(list)
    { 6, &(list)
    { 7, &(list)
    { 8, &(list)
    { 9, NULL }
    }}}}}}}};
  // clang-format on

  printf("\nBBB\n");
  print_list(&lst);

  return 0;
}