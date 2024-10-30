import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args"

// minimistの引数に近い型をparseArgs()とbuildHelp()で共有して使う例
//
// 問題は
// - Optionsをinitiateしたときに、"name" | "version" | "color" という文字列リテラルを指定する必要があること
// - parseArgs()の返り値が {"name": string, "version": string | undefined, "color": boolean} という型にならないこと
// - requiredに存在しないキーを指定してもエラーにならないこと

interface Options<TString extends string, BString extends string> {
    string: TString[]
    boolean: BString[]
    required: (TString | BString)[]
}

function parseArgs<T extends Options<string, string>>(args: string[], options: T) {
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

function buildHelp<T extends Options<string, string>>(options: T): string {
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
    required: ["name", "color"]
});

console.dir(args, { depth: null });
