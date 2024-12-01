const bar = Symbol("bar");

const foo = {
    [Symbol("foo")]: "foo",
    [bar]: "bar",
} as const;

console.log("ob[Symbol('foo')]", foo[Symbol("foo")]); // undefined
console.log("ob[bar]", foo[bar]); // bar

// [deno-ts] Type '{ readonly [x: symbol]: "bar" | "foo"; readonly [bar]: "bar"; }' is not assignable to type 'never'.
// const ob: never = foo;

// 型は自分で付けられる？
const foo2: { readonly [bar]: string } = { [bar]: "bar" };
console.log("foo2[bar]", foo2[bar]); // bar
