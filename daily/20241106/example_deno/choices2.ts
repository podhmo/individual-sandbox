import { buildHelp, parseArgs } from "jsr:@podhmo/with-help@0.3.0";

// 問題:
// ヘルプメッセージに direction の候補が含まれていない
// parsed.direction が DirectionType になっていることを保証したい
// ifでネストが深くなってしまう

const directions = ["north", "south", "east", "west"] as const;
type DirectionType = typeof directions[number];

// type guard function
function isDirectionType(direction: string): direction is DirectionType {
    return directions.includes(direction as DirectionType);
}


const parsed = parseArgs(["--version", "1.0.0"], {
    string: ["version", "direction"],
    required: ["direction"],
    boolean: ["color"],
    alias: { v: "version" },
} as const);

// ここで parsed.direction が DirectionType になっていることを保証したい
if (parsed.direction !== undefined && isDirectionType(parsed.direction)) {
    // let _: never = parsed.direction // stringになってしまうDirectionTypeにしたい
    console.dir(parsed, { depth: null });
} else {
    console.log(buildHelp({})); // ここで help を表示したい
    console.log(`Invalid direction: ${parsed.direction}`);
    Deno.exit(1);
}
