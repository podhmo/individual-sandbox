// for extract literal union
type ExtractLiteralUnion<T extends readonly string[]> = readonly [] extends T ? never : (string extends T[number] ? never : T[number]);


function parse<
    const StringKeys extends readonly string[],
    const CollectKeys extends readonly string[],
>(
    options: {
        string?: StringKeys
        collect: ExtractLiteralUnion<CollectKeys> extends ExtractLiteralUnion<StringKeys> ? CollectKeys : ExtractLiteralUnion<StringKeys>[]
    }
): { string: ExtractLiteralUnion<StringKeys>, collect: ExtractLiteralUnion<CollectKeys>, p: typeof options.collect } {
    return options as any;
}

// [deno-ts] Type '"x"' is not assignable to type '"name" | "version"'.

const _options = parse({
    string: ["name", "version",],
    collect: ["name", "x"],
});
