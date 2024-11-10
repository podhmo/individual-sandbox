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
    const values = { ...options.defaults } as { [K in Booleans[number]]: boolean | undefined } & { [K in Strings[number]]?: string };
    const negatables = (options.negatables || []) as Booleans;
    (options.booleans || []).forEach((key) => {
        if (values[key as Booleans[number]] === undefined) {
            (values as { [K in Booleans[number]]: boolean | undefined })[key as Booleans[number]] = negatables.includes(key);
        }
    });
    return values as { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string };
}

const values = F({
    booleans: ['a', 'b'],
    strings: ['c'],
    defaults: { c: "default" },
    negatables: ['b'],
} as const);
console.dir(values, { depth: null });