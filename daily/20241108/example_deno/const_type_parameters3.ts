// for enfoce as-const assertion
type EnsureLiteralArray<T> = T extends ReadonlyArray<string> ? string[] extends T // if T is not a literal type, return never[]
    ? never[]
    : T
    : never;

type Used<
    StringKey extends string,
    CollectKey extends string,
> = { string: StringKey, collect: CollectKey }

function use<
    const StringKeys extends readonly string[],
    const CollectKeys extends readonly string[]
>(p: {
    string: StringKeys,
    collect?: CollectKeys[number] extends StringKeys[number] ? CollectKeys : never
}): Used<
    StringKeys[number],
    EnsureLiteralArray<typeof p.collect>[number]
> {
    return {} as any;
}

{
    const used = use({ string: ["x", "y", "z"] });
    // [deno-ts] Type 'Used<"x" | "y" | "z", never>' is not assignable to type 'never'.
    let _: never = used;

}
{
    const used = use({ string: ["x", "y", "z"], collect: [] });
    // [deno-ts] Type 'Used<"x" | "y" | "z", never>' is not assignable to type 'never'.    
    let _: never = used;

}
{
    const used = use({ string: ["x", "y", "z"], collect: ["x"] });
    // [deno-ts] Type 'Used<"x" | "y" | "z", "x">' is not assignable to type 'never'.
    let _: never = used;
}

{
    // [deno-ts] Type 'string' is not assignable to type 'never'.
    const used = use({ string: ["x", "y", "z"], collect: ["i"] });
    
    // [deno-ts] Type 'Used<"x" | "y" | "z", never>' is not assignable to type 'never'.
    let _: never = used;
}
