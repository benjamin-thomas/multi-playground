import { describe, expect, test } from "vitest";
import { occurrences, slidingWindow, solve, solve2, sumRows, topmost } from "./main";

/*
npm test watch -- --clearScreen
*/

describe('sliding window', () => {
    test("5 elements by groups of 3", () => {
        expect(slidingWindow([1, 2], 3)).toEqual([]);
        expect(slidingWindow([1, 2, 3], 3)).toEqual([[1, 2, 3]]);
        expect(slidingWindow([1, 2, 3, 4], 3)).toEqual([[1, 2, 3], [2, 3, 4]]);
        expect(slidingWindow([1, 2, 3, 4, 5], 3)).toEqual([[1, 2, 3], [2, 3, 4], [3, 4, 5]]);
    });
});

describe("occurrences", () => {
    test("none", () => {
        expect(occurrences([])).toEqual(new Map());
    });

    test("many", () => {
        expect(occurrences([1, 2, 1, 3, 2, 1])).toEqual(
            new Map()
                .set(1, 3)
                .set(2, 2)
                .set(3, 1)
        );
    });

    test("topmost", () => {
        expect(topmost(2, (a, b) => b - a, new Map()
            .set(1, 3)
            .set(2, 2)
            .set(3, 1)
            .set(4, 2)
        )).toEqual<Array<[number, number]>>([[1, 3], [4, 2]]);
    });

    test("sum", () => {
        const input: Array<[number, number]> = [[2, 3], [4, 5]];

        // 2*3 + 4*5 = 26
        expect(sumRows(input)).toEqual(26);
    });
});


describe("solve", () => {
    /*
    Input: nums = [1,1,2,2,3,4,2,3], k = 6, x = 2

    Output: [6,10,12]

    irb(main):001:0> 6 + 10 + 12
    => 28
     */
    test("example 1", () => {
        expect(solve([1, 1, 2, 2, 3, 4, 2, 3], 6, 2)).toEqual(28);
        expect(solve2([1, 1, 2, 2, 3, 4, 2, 3], 6, 2)).toEqual([6, 10, 12]);
    });

    /*
    Input: nums = [3,8,7,8,7,5], k = 2, x = 2

    Output: [11,15,15,15,12]

    irb(main):002:0> [11,15,15,15,12].sum
    => 68
     */
    test("example 2", () => {
        expect(solve([3, 8, 7, 8, 7, 5], 2, 2)).toEqual(68);
        expect(solve2([3, 8, 7, 8, 7, 5], 2, 2)).toEqual([11, 15, 15, 15, 12]);
    });
});


