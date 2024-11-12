export function slidingWindow<T>(items: T[], size: number): T[][] {
    const result: T[][] = [];
    for (let i = 0; i <= items.length - size; i++) {
        const slice = items.slice(i, i + size);
        result.push(slice);
    }
    return result;
}

export function occurrences<T>(items: T[]): Map<T, number> {
    const result = new Map<T, number>();
    for (const item of items) {
        const count = result.get(item) || 0;
        result.set(item, count + 1);
    }
    return result;
}

export function topmost<V>(n: number, compare: (a: V, b: V) => number, dict: Map<number, V>): Array<[number, V]> {
    return Array.from(dict.entries()).sort(([ka, va], [kb, vb]) => {
        return va === vb ? kb - ka : compare(va, vb);
    }).slice(0, n);
}

export function sumRows(rows: [number, number][]): number {
    return rows.reduce((acc, [a, b]) => acc + a * b, 0);
}

export function solve(items: number[], k: number, x: number): number {
    const compareValues = (a: number, b: number) => b - a;
    return slidingWindow(items, k)
        .map((window) => topmost(x, compareValues, occurrences(window)))
        .reduce((acc, rows) => acc + sumRows(rows), 0);
}

export function solve2(items: number[], k: number, x: number): number[] {
    const compareValues = (a: number, b: number) => b - a;
    const result: number[] = [];
    slidingWindow(items, k)
        .map((window) => topmost(x, compareValues, occurrences(window)))
        .forEach((rows) => result.push(sumRows(rows)));
        
    return result;
}