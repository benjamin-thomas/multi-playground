#include <stdio.h>
#include <stdlib.h>

/*
 * https://www.learn-c.org/en/Linked_lists
 */
typedef struct node {
    int val;
    struct node *next;
} node_t;

void print_list(node_t *head) {
    node_t *curr = head;

    while (curr != NULL) {
        printf("%d\n", curr->val);
        curr = curr->next;
    }
}

//void push(node_t *head, int val) {
//    node_t *curr =head;
//    while (curr->next != NULL) {
//        curr = curr->next;
//    }
//    // Now we can add the item
//    curr->next = malloc(sizeof(node_t)); // TODO: error handling?
//    curr->next->val = val;
//    curr->next->next = NULL;
//}

void push(node_t **head, int val) {
    node_t *new_node = malloc(sizeof(node_t));
    new_node->val = val;
    new_node->next = *head;
    *head = new_node;
}

int pop(node_t **head) {
    int retVal = -1;
    node_t *next_node = NULL;

    if (*head == NULL) return -1;
    next_node = (*head)->val;
    free(*head);
    *head = next_node;

    return retVal;
};

//static int create_list() {
//    node_t *head = NULL;
//    head = malloc(sizeof(node_t));
//    if (head == NULL) return 1; // allocation failed
//
//    head->val = 1;
//    head->next = malloc(sizeof(node_t));
//    if (head->next == NULL) return 1; // allocation failed
//    head->next->val = 2;
//    head->next->next = NULL;
//    return 0;
//}