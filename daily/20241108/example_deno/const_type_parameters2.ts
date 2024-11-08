function use<const T extends readonly string[]>(params: { readonly xs: T }): { [P in T[number]]?: string } {
    const ob: { [P in T[number]]?: string } = {}
    for (const x of params.xs) {
        ob[x as (typeof params.xs)[number]] = x;
    }
    return ob
}

const used = use({ xs: ["x", "y", "z"] });
let _: never = used;
console.dir(used, { depth: null })
