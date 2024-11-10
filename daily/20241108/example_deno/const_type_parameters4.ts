type Touch<T> = T extends ReadonlyArray<string> ? string[] extends T ? "widen" : "ok" : T;
function F<T extends readonly string[]>(xs: T): { xs: Touch<typeof xs> } {
    return {} as any
}
{

    // [deno-ts] Type '{ xs: "ok"; }' is not assignable to type 'never'.
    let _: never = F([])

    // [deno-ts] Type '{ xs: "widen"; }' is not assignable to type 'never'.
    let _: never = F()

    // [deno-ts] Type '{ xs: "widen"; }' is not assignable to type 'never'.
    let _: never = F(undefined)

    // [deno-ts] Type '{ xs: "widen"; }' is not assignable to type 'never'.
    let _: never = F(["x"])

    // [deno-ts] Type '{ xs: "ok"; }' is not assignable to type 'never'.
    let _: never = F(["x", "y"] as const)

    // [deno-ts] Type '{ xs: "widen"; }' is not assignable to type 'never'.
    const xs: string[] = ["x", "y"];
    let _: never = F(xs)

    // [deno-ts] Type '{ xs: "widen"; }' is not assignable to type 'never'.
    let _: never = F([1, 2, 3])

    // [deno-ts] Type '{ xs: "widen"; }' is not assignable to type 'never'.
    let _: never = F([1, 2, 3] as const)
}


function G<T extends readonly string[]>(xs?: T): { xs: Touch<typeof xs> } {
    return {} as any
}
{
    // [deno-ts] Type '{ xs: "ok"|undefined; }' is not assignable to type 'never'.
    let _: never = G([])

    // [deno-ts] Type '{ xs: "widen"|undefined; }' is not assignable to type 'never'.
    let _: never = G()

    // [deno-ts] Type '{ xs: "widen"|undefined; }' is not assignable to type 'never'.
    let _: never = G(undefined)

    // [deno-ts] Type '{ xs: "widen"|undefined; }' is not assignable to type 'never'.
    let _: never = G(["x"])

    // [deno-ts] Type '{ xs: "ok"|undefined; }' is not assignable to type 'never'.
    let _: never = G(["x", "y"] as const)

    // [deno-ts] Type '{ xs: "widen"|undefined; }' is not assignable to type 'never'.
    const xs: string[] = ["x", "y"];
    let _: never = G(xs)

    // [deno-ts] Type '{ xs: "widen"|undefined; }' is not assignable to type 'never'.
    let _: never = G([1, 2, 3])

    // [deno-ts] Type '{ xs: "widen"|undefined; }' is not assignable to type 'never'.
    let _: never = G([1, 2, 3] as const)
}

type Touch2<T> = T extends undefined ? "??" : Touch<Exclude<T, undefined>>
function H<T extends readonly string[]>(xs?: T): { xs: Touch2<typeof xs> } {
    return {} as any
}
{
    // [deno-ts] Type '{ xs: "ok"|"??"; }' is not assignable to type 'never'.
    let _: never = H([])

    // [deno-ts] Type '{ xs: "widen"|"??"; }' is not assignable to type 'never'.
    let _: never = H()

    // [deno-ts] Type '{ xs: "widen"|"??"; }' is not assignable to type 'never'.
    let _: never = H(undefined)

    // [deno-ts] Type '{ xs: "widen"|"??"; }' is not assignable to type 'never'.
    let _: never = H(["x"])

    // [deno-ts] Type '{ xs: "ok"|"??"; }' is not assignable to type 'never'.
    let _: never = H(["x", "y"] as const)

    // [deno-ts] Type '{ xs: "widen"|"??"; }' is not assignable to type 'never'.
    const xs: string[] = ["x", "y"];
    let _: never = H(xs)

    // [deno-ts] Type '{ xs: "widen"|"??"; }' is not assignable to type 'never'.
    let _: never = H([1, 2, 3])

    // [deno-ts] Type '{ xs: "widen"|"??"; }' is not assignable to type 'never'.
    let _: never = H([1, 2, 3] as const)
}

// - 省略      - - ["x", "y", "z"], undefined
// - 明示的に空  -- ["x", "y", "z"], []
// - subset     -- ["x", "y", "z"], ["x"]
// - not subset -- ["x", "y", "z"], ["i"]
type EnsureLiteralArray<T> = T extends ReadonlyArray<string> ? (string[] extends T ? never[] : T) : never;
function Correct<const T extends readonly string[]>(xs?: T): { xs: EnsureLiteralArray<typeof xs> } {
    return {} as any
}

{
    // [deno-ts] Type '{ xs: readonly []; }' is not assignable to type 'never'.
    let _: never = Correct([])

    // [deno-ts] Type '{ xs: never[]; }' is not assignable to type 'never'.
    let _: never = Correct()

    // [deno-ts] Type '{ xs: never[]; }' is not assignable to type 'never'.
    let _: never = Correct(undefined)

    // [deno-ts] Type '{ xs: readonly ["x"]; }' is not assignable to type 'never'.
    let _: never = Correct(["x"])

    // [deno-ts] Type '{ xs: readonly ["x", "y"]; }' is not assignable to type 'never'.
    let _: never = Correct(["x", "y"] as const)

    // [deno-ts] Type '{ xs: never[]; }' is not assignable to type 'never'.
    const xs: string[] = ["x", "y"];
    let _: never = Correct(xs)
}

function X<
    const T extends readonly string[],
    const G extends readonly string[]
>(
    options: { xs?: T extends G ? T : never, ys?: G }
): ensureLiteralUnion<T> {
    return options as any
}

type ensureLiteralUnion<T extends readonly string[]> = readonly [] extends T ? { widen1: never } : (string extends T[number] ? { widen2: never } : { ok: T[number] })

const ys = ["x", "y", "z"] as const
let _: never = X({ ys })                       // { widen1: never; }
let _: never = X({ ys, xs: [] })               // { widen1: never; }
let _: never = X({ ys, xs: undefined })        // { widen1: never; }
let _: never = X({ ys, xs: ["x"] })            // { ok: "x"; }
let _: never = X({ ys, xs: ["a", "b"] })       // { ok: "a" | "b"; }
let _: never = X({ ys, xs: ["a", "b", "c"] })  // { ok: "a" | "b" | "c"; }
const xs: string[] = ["x"];
let _: never = X({ ys, xs })                    // { widen2: never; }
