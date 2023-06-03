import { some, none, add, mul } from "../src/v2";

describe('v2', () => {
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
});
