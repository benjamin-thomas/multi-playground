import {expect, test} from "bun:test";

/*
 *   bun test --watch
 */

/*
 * 01 - Return the last element of a list
 */
function last<T>(lst: Array<T>): T | undefined {
    return lst[lst.length - 1];
}

test("01", () => {
    expect(last([])).toStrictEqual(undefined);
    expect(last([1, 2, 3])).toStrictEqual(3);
});


/*
 * 02 - Return the last two elements of a list (a tuple)
 */

function last2<T>(lst: Array<T>): [T, T] | undefined {
    const a = lst[lst.length - 1];
    const b = lst[lst.length - 2];
    if (a && b)
        return [b, a];
}

test("02", () => {
    expect(last2([])).toStrictEqual(undefined);
    expect(last2([1])).toStrictEqual(undefined);
    expect(last2([1, 2, 3])).toStrictEqual([2, 3]);
});
