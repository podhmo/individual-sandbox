import { buildHelp, parseArgs as original } from "jsr:@podhmo/with-help@0.3.0";

// 問題:
// 型の導出が死んでる (neverになってしまう)

const directions = ["north", "south", "east", "west"] as const;
type DirectionType = typeof directions[number];

function parseArgs(args: string[], options: Parameters<typeof original>[1]): ReturnType<typeof original> & { direction: DirectionType } {
    const parsed = original(args, options) as ReturnType<typeof original> & { direction: DirectionType };
    if (parsed.direction !== undefined && !directions.includes(parsed.direction as DirectionType)) {
        console.log(buildHelp(options));
        console.log(`Invalid direction: ${parsed.direction}`);
        Deno.exit(1);
    }
    return parsed
}

const parsed = parseArgs(["--version", "1.0.0"], {
    string: ["version", "direction"], // Type 'string' is not assignable to type never.
    required: ["direction"],
    boolean: ["color"], // Type 'boolean' is not assignable to type never.
    alias: { v: "version" },
} as const);
console.dir(parsed, { depth: null });