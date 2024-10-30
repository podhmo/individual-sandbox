import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args"

// 問題は
// - requiredに存在しないキーを指定してもstring,booleanにas constを与えないとエラーにならないこと


interface Options<TStrings extends readonly string[], BStrings extends readonly string[]> {
    string: TStrings
    boolean: BStrings
    required: (TStrings[number] | BStrings[number])[]
}

function parseArgs<TStrings extends string[], BStrings extends string[]>(
    args: string[],
    options: Options<TStrings, BStrings>
) {
    if (args.includes("--help")) {
        console.log(buildHelp(options));
        Deno.exit(0);
    }
    const parsed = originalParseArgs(args, options);
    for (const key of options.required) {
        if (parsed[key] === undefined) {
            console.error(`--${key} is required`);
            Deno.exit(1);
        }
    }
    return parsed;
}

function buildHelp<TStrings extends string[], BStrings extends string[]>(
    options: Options<TStrings, BStrings>
) {
    const lines = [];
    for (const key of options.string) {
        lines.push(`--${key} <string>${options.required.includes(key) ? " (required)" : ""}`);
    }
    for (const key of options.boolean) {
        lines.push(`--${key}${options.required.includes(key) ? " (required)" : ""}`);
    }
    return lines.join("\n");
}


// deno run --allow-all shared-type.ts --name foo --version 1.0 --color

// 型チェックの結果が嬉しくない {"name": string, "version": string | undefined, "color": boolean} になると嬉しい
// [deno-ts] Type '{ [x: string]: undefined; _: (string | number)[]; }' is not assignable to type 'never'.
// const args: never = parseArgs(Deno.args, options);

const args = parseArgs(Deno.args, {
    string: ["name", "version"],
    boolean: ["color"],
    required: ["name", "color", "x"]
}); // as constを付けずともrequiredをしっかりcheckしてほしい
// } as const); // as constを付けずともrequiredをしっかりcheckしてほしい

console.dir(args, { depth: null });
