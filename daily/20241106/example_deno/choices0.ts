import { parseArgs as original } from "jsr:@std/cli@1.0.6"

// 問題:
// こういう関数を返す関数を作りたい

const directions = ["north", "south", "east", "west"] as const;
type DirectionType = typeof directions[number];

function parseArgs(args: string[], options: Parameters<typeof original>[1]): ReturnType<typeof original> & { direction: DirectionType } {
    const parsed = original(args, options)
    if (parsed.direction === undefined) {
        throw new Error("version is required");
    } else if (!directions.includes(parsed.direction as DirectionType)) {
        throw new Error(`Invalid direction: ${parsed.direction}`);
    }
    return parsed as ReturnType<typeof original> & { direction: DirectionType };
}

// use this
const parsed = parseArgs(["--version", "1.0.0"], {
    string: ["version"],
    boolean: ["color"],
    alias: { v: "version" },
} as const);

parsed // { version: string; color: boolean; direction: DirectionType; }
parsed.direction // DirectionType
console.dir(parsed, { depth: null });

