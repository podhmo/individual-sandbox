import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args";

// 問題:
// - vscodeで型名を確認したときに、直接展開された型名が表示されると嬉しい


// リテラル型配列に限定するためのユーティリティ型
type EnsureLiteralArray<T> = T extends ReadonlyArray<string>
    ? string[] extends T // リテラル型でない場合は never[] にする
    ? never[]
    : T
    : never;

interface Options<SStrings extends readonly string[], BStrings extends readonly string[], RStrings extends readonly string[]> {
    string: EnsureLiteralArray<SStrings>;
    boolean: EnsureLiteralArray<BStrings>;
    required: EnsureLiteralArray<RStrings[number] extends (SStrings[number] | BStrings[number]) ? RStrings : never>;
}

type ParsedArgs<T extends Options<any, any, any>> = {
    [K in T['string'][number]]: K extends T['required'][number]
    ? string
    : string | undefined;
} & {
    [K in T['boolean'][number]]: K extends T['required'][number]
    ? boolean
    : boolean | undefined;
};

// パース関数の定義
function parseArgs<SStrings extends readonly string[], BStrings extends readonly string[], RStrings extends readonly string[]>(
    args: string[],
    options: Options<SStrings, BStrings, RStrings>
): ParsedArgs<Options<SStrings, BStrings, RStrings>> {
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
    return parsed as ParsedArgs<Options<SStrings, BStrings, RStrings>>;
}

// ヘルプをビルドする関数
function buildHelp<SStrings extends readonly string[], BStrings extends readonly string[], RStrings extends readonly string[]>(
    options: Options<SStrings, BStrings, RStrings>
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

// [deno-ts] Type 'ParsedArgs<Options<["name", "version"], ["color"], ["name", "color"]>>' is not assignable to type 'never'.
console.log(args.name, args.version, args.color);
