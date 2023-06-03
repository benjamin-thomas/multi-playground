export function add(a?: number, b?: number): undefined | number {
    if (a)
        if (b)
            return a + b;
}