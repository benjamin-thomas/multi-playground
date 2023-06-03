import {some, none, add} from "../src/v2";

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
});
