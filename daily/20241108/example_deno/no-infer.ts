const directions = ["north", "east", "south", "west"] as const;
type Direction = typeof directions[number];

function include<T extends string>(xs:  T[], x: T): x is T {
    return xs.includes(x);
}

const xs = directions as unknown as Direction[]; // readonlyを外したほうが分かりやすそうだった
include(xs, "north"); // true
// 型チェックでエラーにしたいが、エラーにならない
include(xs, "up"); // false 

function include2<T extends string>(xs:  T[], x: NoInfer<T>): x is T{
    return xs.includes(x);
}

include2(xs, "north"); // true
// [deno-ts] Argument of type '"up"' is not assignable to parameter of type '"north" | "east" | "south" | "west"'.
include2(xs, "up"); // false

// or alternative
function include3<T extends string, T1 extends T>(xs:  T[], x: T1): x is T1 {
    return xs.includes(x);
}

include3(xs, "north"); // true
// [deno-ts] Argument of type '"up"' is not assignable to parameter of type '"north" | "east" | "south" | "west"'.
include3(xs, "up"); // false
