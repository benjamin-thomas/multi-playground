import {describe, expect, test} from "bun:test";

/*
    bun test --watch
*/

// 1 - Return the last element of a list
describe("Suite", () => {

    test("Exercise 01", () => {
        function last<T>(lst: Array<T>): T {
            return lst[lst.length - 1];
        }

        expect(last([])).toBe(undefined);
        expect(last([1, 2, 3])).toBe(3);
    });
});
