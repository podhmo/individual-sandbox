const colors = ["red", "green", "blue"] as const;
type Color = typeof colors[number];

const directions = ["north", "south", "east", "west"] as const;
type Direction = typeof directions[number];

// だいたいは以下の様なコードになる。ob2,ob3のtypoとかがちょっと嫌。
function use1() {
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;

    if (!colors.includes(ob.color)) {
        throw new Error("invalid color");
    }
    const ob2: Omit<typeof ob, "color"> & { color: Color } = ob;

    if (!directions.includes(ob.direction)) {
        throw new Error("invalid direction");
    }
    const ob3: Omit<typeof ob2, "direction"> & { direction: Direction } = ob2;

    // [deno-ts] Type 'Omit<Omit<{ readonly name: "foo"; readonly color: "red"; readonly direction: "north"; }, "color"> & { color: "red" | "green" | "blue"; }, "direction"> & { direction: "north" | ... 2 more ... | "west"; }' is not assignable to type 'never'.
    // const _ : never= ob3

    const ob4 = { ...ob3 };
    // [deno-ts] Type '{ name: "foo"; color: Color; direction: Direction; }' is not assignable to type 'never'.
    // const _ : never= ob4

    console.dir(ob4);
}

function isColor(arg: string): arg is Color {
    return colors.includes(arg as Color);
}
function isDirection(arg: string): arg is Direction {
    return directions.includes(arg as Direction);
}

// type-guardが使えるならまだマシ
function use2() {
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;

    if (!isColor(ob.color)) {
        throw new Error("invalid color");
    }
    if (!isDirection(ob.direction)) {
        throw new Error("invalid direction");
    }

    // [deno-ts] Type '{ readonly name: "foo"; readonly color: "red"; readonly direction: "north"; }' is not assignable to type 'never'.
    // const _ : never= ob

    console.dir(ob);
}

// shadowingで書く方法もあるがネストが深くなると辛い (と思ったらそもそもエラーになる)
function use3() {
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;
    {
        const ob = { ...ob, colors: "red" as Color } as const; // [deno-ts] 'ob' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer.
        {
            const ob = { ...ob, direction: "north" as Direction }; // [deno-ts] 'ob' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer.
            {
                const _: never = ob;
                console.dir(ob);
            }
        }
    }
}

// CPS。これはネタ回答。
function use4() {
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;

    contWithColor(ob, (ob) => {
        contWithDirection(ob, (ob) => {
            // [deno-ts] Type '{ readonly name: "foo"; readonly color: "red"; readonly direction: "north"; } & { color: "red" | "green" | "blue"; } & { direction: "north" | "south" | "east" | "west"; }' is not assignable to type 'never'.
            // const _: never = ob;
            console.dir(ob);
        });
    });
}

function contWithColor<
    T extends Record<string, string>,
    R extends T & { color: Color },
>(
    ob: T,
    cont: (_: R) => unknown,
): unknown {
    if (ob.color === undefined) {
        throw new Error("color is required");
    }
    if (!isColor(ob.color)) {
        throw new Error("invalid color");
    }
    const _: never = ob.color; // ここでob.colorがColorになっていることを保証したい
    return cont(ob as unknown as R); // 無理っぽいのでas unknown as R
}

function contWithDirection<
    T extends Record<string, string>,
    R extends T & { direction: Direction },
>(
    ob: T,
    cont: (_: R) => unknown,
): unknown {
    if (!isDirection(ob.direction)) {
        throw new Error("invalid direction");
    }
    return cont({ ...ob, direction: ob.direction as Direction } as R);
}

use1();
use2();
use3();
use4();
