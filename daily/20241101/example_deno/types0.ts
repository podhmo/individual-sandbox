function F<
    Booleans extends readonly string[],
    Strings extends readonly string[],
    TDefaults extends { [K in Booleans[number]]?: boolean } & { [K in Strings[number]]?: string },
>(
    options: {
        booleans: Booleans,
        strings: Strings,
        defaults: TDefaults,
        negatables?: Booleans,
    },
): { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string } {
    const values = { ...options.defaults }
    const negatables = options.negatables || [];
    (options.booleans || []).forEach((key) => {
        if (values[key] === undefined) {
            values[key] = negatables.includes(key);
        }
    });
    return values;
}

const values = F({
    booleans: ['a', 'b'],
    strings: ['c'],
    defaults: { c: "default" },
    negatables: ['b'],
} as const);
console.dir(values, { depth: null });