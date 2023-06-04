import { some, none, add, mul, addMore } from "../src/v2b";

describe('v2b simulates exhaustive pattern matching', () => {
    it('add', () => {
        expect(
            add(none, none)
        ).toEqual(none);

        expect(
            add(some(1), none)
        ).toEqual(none);

        expect(
            add(none, some(2))
        ).toEqual(none);

        expect(
            add(some(1), some(2))
        ).toEqual(some(3));
    });

    it('multiplies, Elm style!', () => {
        expect(mul(none, none)).toEqual(none);
        expect(mul(some(3), none)).toEqual(none);
        expect(mul(none, some(4))).toEqual(none);
        expect(mul(some(3), some(4))).toEqual(some(12));
    });

    it('adds more with map3', () => {
        expect(addMore(none, none, none)).toEqual(none);
        expect(addMore(some(1), none, none)).toEqual(none);
        expect(addMore(none, some(2), none)).toEqual(none);
        expect(addMore(none, none, some(3))).toEqual(none);
        expect(addMore(some(1), some(2), some(3))).toEqual(some(6));
    });
});
