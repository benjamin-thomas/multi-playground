#include <stdio.h>


/*

gcc -g main.c -o /tmp/tmp && cat ./input.txt | /tmp/tmp

gdb -tui /tmp/tmp
(gdb) break 29
(gdb) run < ./input.txt
(gdb) display i j nums
(gdb) undisplay i j


*/

int main() {
    int times;
    scanf("%d", &times);
    for (int i = 0; i < times; i++) {
        int n, k;
        scanf("%d %d", &n, &k);

        // Step 1: populate the array
        int nums[n];
        for (int j = 0; j < n; j++) {
            scanf("%d", &nums[j]);
        }

        // Step 2: build the right part
        int nums2[n];
        int idx = 0;
        for (int j = n - k; j < n; j++) {
            nums2[idx++] = nums[j];
        }

        // Step 3: build the left part
        for (int j = 0; j < n - k; j++) {
            nums2[idx++] = nums[j];
        }

        // Step 4: print the new array
        for (int j = 0; j < n; j++) {
            printf("%d ", nums2[j]);
        }
        printf("\n");
    }
    return 0;
}