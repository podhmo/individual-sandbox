
function use<T extends string[]>(xs: T): { xs: T[number][] } {
    return { xs: xs }
}

{
    const _options = use(["x", "y", "z"])
    // let _: never = _options // [deno-ts] Type '{ xs: string[]; }' is not assignable to type 'never'.
}
{
    const _options = use(["x", "y", "z"] as const)
    // let _: never = _options // [deno-ts] Type '{ xs: ("x" | "y" | "z")[]; }' is not assignable to type 'never'.
}

function use2<const T extends string[]>(xs: T): { xs: T } {
    return { xs: xs }
}
{
    const _options = use2(["x", "y", "z"])
    // let _: never = _options // [deno-ts] Type '{ xs: ["x", "y", "z"]; }' is not assignable to type 'never'.
}
{
    const _options = use2(["x", "y", "z"] as const)
    // let _: never = _options // [deno-ts] Type '{ xs: ["x", "y", "z"]; }' is not assignable to type 'never'.
}

function use3<const T extends string[]>(xs: T): { xs: T[number][] } {
    return { xs: xs }
}
{
    const _options = use3(["x", "y", "z"])
    // let _: never = _options // [deno-ts] Type '{ xs: ("x" | "y" | "z")[]; }' is not assignable to type 'never'.
}
{
    const _options = use3(["x", "y", "z"])
    // let _: never = _options // [deno-ts] Type '{ xs: ("x" | "y" | "z")[]; }' is not assignable to type 'never'.
}
