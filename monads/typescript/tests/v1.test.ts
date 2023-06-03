import { add } from '../src/v1';

describe('v1', () => {
    it('add', () => {
        expect(add(undefined, undefined)).toEqual(undefined);
        expect(add(undefined, 2)).toEqual(undefined);
        expect(add(1, undefined)).toEqual(undefined);
        expect(add(1, 2)).toEqual(3);
    });
});
