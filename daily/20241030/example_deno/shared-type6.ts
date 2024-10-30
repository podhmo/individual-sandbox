import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args";

// 問題:
// - vscodeで型名を確認したときに、直接展開された型名が表示されると嬉しい

// リテラル型配列に限定するためのユーティリティ型
type EnsureLiteralArray<T> = T extends ReadonlyArray<string>
    ? string[] extends T // リテラル型でない場合は never[] にする
    ? never[]
    : T
    : never;

type Parsed<StringKeys extends readonly string[], BooleanKeys extends readonly string[], RequiredKeys extends readonly string[]> = {
    [K in StringKeys[number]]: K extends RequiredKeys[number]
    ? string
    : string | undefined;
} & {
    [K in BooleanKeys[number]]: K extends RequiredKeys[number]
    ? boolean
    : boolean | undefined;
} & { _: string[] };

// パース関数の定義
function parseArgs<StringKeys extends readonly string[], BooleanKeys extends readonly string[], RequiredKeys extends readonly string[]>(
    args: string[],
    options: {
        string: EnsureLiteralArray<StringKeys>;
        boolean: EnsureLiteralArray<BooleanKeys>;
        required: EnsureLiteralArray<RequiredKeys[number] extends (StringKeys[number] | BooleanKeys[number]) ? RequiredKeys : never>;
    }
): Parsed<StringKeys, BooleanKeys, RequiredKeys> {
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
    return parsed as Parsed<StringKeys, BooleanKeys, RequiredKeys>;
}

// ヘルプをビルドする関数
function buildHelp(
    options: {
        string: string[];
        boolean: string[];
        required: string[];
    }
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

// コマンドライン引数を解析する
const args = parseArgs(Deno.args, {
    string: ["name", "version"],
    boolean: ["color"],
    required: ["name", "color"]
} as const);

console.dir(args, { depth: null });

// [deno-ts] Type 'Parsed<["name", "version"], ["color"], ["name", "color"]>' is not assignable to type 'never'.
// const x: never = args;
console.log(args.name, args.version, args.color);
