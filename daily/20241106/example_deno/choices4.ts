import { parseArgs, buildHelp } from "jsr:@podhmo/with-help@0.3.0";

const directions = ["north", "south", "east", "west"] as const;
type DirectionType = typeof directions[number];

function requireChoices<
    T extends Record<string, unknown>, K extends string, Opt extends { [P in keyof Opt]: readonly K[] }
>(ob: T, options: Opt): { [P in keyof Opt]: K } & Omit<T, keyof Opt> {
    // assert length of options == 1
    for (const [key, candidates] of Object.entries(options) as unknown as [string, K[]]) {
        if (ob[key] !== undefined && !candidates.includes(ob[key] as K)) {
            console.log(buildHelp(options));
            console.log(`Invalid direction: ${ob[key]}`);
            Deno.exit(1);
        }
    }
    return ob as { [P in keyof Opt]: K } & Omit<T, keyof Opt>;
}


const parsed = parseArgs(["--version", "1.0.0"], {
    string: ["version", "direction"],
    required: ["direction"],
    boolean: ["color"],
    alias: { v: "version" },
} as const);

// directionの型がstringになってしまう
const parsed2: typeof parsed & { direction: DirectionType } = requireChoices(parsed, { direction: directions } );
// let _: never = parsed2;



