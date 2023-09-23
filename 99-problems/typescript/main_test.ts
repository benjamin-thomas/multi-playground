import { expect, test } from "bun:test";

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



/*
 * 03 - N'th Element of a List
 */
function nth<T>(lst: Array<T>, idx: number): T {
    return lst[idx];
}

test("03", () => {
    expect(nth([], 0)).toStrictEqual(undefined);
    expect(nth([1, 2, 3], 0)).toStrictEqual(1);
});



/*
* 04 - Find the number of elements of a list --> SKIP ([1,2,3].length === 3)
*/
function len<T>(lst: Array<T>): number {
    return lst.length;
}

test("04", () => {
    expect(len([])).toStrictEqual(0);
    expect(len([1, 2, 3])).toStrictEqual(3);
});



/*
 * 05 - Reverse a List
 */
function rev<T>(lst: Array<T>): Array<T> {
    return lst.reverse();
}

test("05", () => {
    expect(rev([])).toStrictEqual([]);
    expect(rev([1, 2, 3])).toStrictEqual([3, 2, 1]);
});



/*
 * 06 - Find out whether a list is a palindrome.
 */
function isPalindrome(lst: Array<unknown>) {
    const rev = lst.toReversed();
    return lst.every((v, i) => v === rev[i]);
}

test("06", () => {
    expect(isPalindrome([])).toStrictEqual(true);
    expect(isPalindrome(["g", "o"])).toStrictEqual(false);
    expect(isPalindrome(["l", "e", "v", "e", "l"])).toStrictEqual(true);
});
