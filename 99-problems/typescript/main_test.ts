import {expect, test} from "bun:test";

/*
 *   bun test --watch
 */

/*
 * 01 - Return the last element of a list"
 */
function last<T>(lst: Array<T>): T {
    return lst[lst.length - 1];
}

test("01", () => {
    expect(last([])).toBe(undefined);
    expect(last([1, 2, 3])).toBe(3);
});
