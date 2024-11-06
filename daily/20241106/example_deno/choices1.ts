import { parseArgs as original } from "jsr:@std/cli@1.0.6";

// 問題:
// 型の記述が煩雑すぎる（ついでにまだ壊れている）

type ParseArgsFuncType = typeof original;

const directions = ["north", "south", "east", "west"] as const;
type DirectionType = typeof directions[number];


function WithDirectionValidation<
    TFactory extends ParseArgsFuncType,
>(factory: TFactory): (...params: Parameters<TFactory>) => ReturnType<TFactory> & { direction: DirectionType } {
    return (args: string[], options?: Parameters<TFactory>[1]) => {
        const parsed = factory(args, options);
        if (parsed.direction === undefined) {
            throw new Error(`direction is required, args: ${JSON.stringify(args)}`);
        } else if (!directions.includes(parsed.direction as DirectionType)) {
            throw new Error(`Invalid direction: ${parsed.direction}, args: ${JSON.stringify(args)}`);
        }
        return { ...parsed, direction: parsed.direction as DirectionType };
    }
}

const parseArgs = WithDirectionValidation(original)

// use this
const parsed = parseArgs(["--version", "1.0.0", "--direction", "north"], {
    string: ["version"],
    boolean: ["color"],
    alias: { v: "version" },
} as const);

parsed.direction // DirectionType
console.dir(parsed, { depth: null });

