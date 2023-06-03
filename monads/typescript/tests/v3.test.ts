import {ok, err, add, mul} from '../src/v3';

describe('v3', () => {
    it('add', () => {
        expect(
            add(err("1st failed"), err("2nd failed"))
        ).toEqual(err("1st failed"));

        expect(
            add(err("1st failed"), ok(2))
        ).toEqual(err("1st failed"));

        expect(
            add(ok(1), err("2nd failed"))
        ).toEqual(err("2nd failed"));

        expect(
            add(ok(1), ok(2))
        ).toEqual(ok(3))
    });

    it('mul', () => {
        expect(
            mul(err("1st failed"), err("2nd failed"))
        ).toEqual(err("1st failed"));

        expect(
            mul(err("1st failed"), ok(2))
        ).toEqual(err("1st failed"));

        expect(
            mul(ok(1), err("2nd failed"))
        ).toEqual(err("2nd failed"));

        expect(
            mul(ok(3), ok(4))
        ).toEqual(ok(12))
    });

});
