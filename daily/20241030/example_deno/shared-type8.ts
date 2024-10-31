import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args";

// 問題:
// - originalParseArgs()を呼び出す部分で型が合わない


// リテラル型配列に限定するためのユーティリティ型
type EnsureLiteralArray<T> = T extends ReadonlyArray<string>
    ? string[] extends T // リテラル型でない場合は never[] にする
    ? never[]
    : T
    : never;

type Parsed<
    StringKey extends string,
    BooleanKey extends string,
    RequiredKey extends string,
    CollectKey extends string,
    DefaultKey extends string
> = {
    [K in StringKey]:
    K extends CollectKey
    ? string[]
    : (K extends (RequiredKey | DefaultKey) ? string : (string | undefined))
} & {
        [K in BooleanKey]: K extends RequiredKey
        ? boolean
        : (K extends DefaultKey ? boolean : boolean | undefined);
    } & { _: string[] };


// パース関数の定義
function parseArgs<
    StringKeys extends readonly string[],
    BooleanKeys extends readonly string[],
    RequiredKeys extends readonly string[],
    CollectKeys extends readonly string[],
    TDefaults extends { [P in StringKeys[number]]?: string | string[]},
    DefaultKey extends Extract<keyof TDefaults, string>,
>(
    args: string[],
    options: {
        string: EnsureLiteralArray<StringKeys>;
        boolean: EnsureLiteralArray<BooleanKeys>;
        collect?: EnsureLiteralArray<CollectKeys>[number] extends StringKeys[number] ? CollectKeys : never;
        required: EnsureLiteralArray<RequiredKeys[number] extends (StringKeys[number] | BooleanKeys[number]) ? RequiredKeys : never>;
        default?: TDefaults // どうやらこれではdefaultは好きな値をいれられるらしい
    }
): Parsed<StringKeys[number], BooleanKeys[number], RequiredKeys[number], CollectKeys[number], DefaultKey> {

    if (args.includes("--help")) {
        console.log(buildHelp(options));
        Deno.exit(0);
    }
    const defaults: Record<string, unknown> = options.default || {}
    for (const key of (options.collect || [])) {
        if (defaults[key] === undefined) {
            defaults[key] = [];
        }
    }
    // @ts-ignore options["default"] is conflicted?
    // [deno-ts] Type 'Record<string, unknown>' is not assignable to type 'undefined'.
    const parsed = originalParseArgs(args, { ...options, default: defaults });

    for (const key of options.required) {
        if (parsed[key] === undefined) {
            console.error(`--${key} is required`);
            Deno.exit(1);
        }
    }
    return parsed as Parsed<StringKeys[number], BooleanKeys[number], RequiredKeys[number], CollectKeys[number], DefaultKey>;
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
    string: ["name", "version", "nickname", "items"],
    collect: ["items"],
    boolean: ["color"],
    required: ["name", "color"],
    default: { nickname: "------" },
} as const);

console.dir(args, { depth: null });


// [deno-ts] Type 'Parsed<"name" | "version" | "nickname" | "items", "color", "name" | "color", "items", "nickname" | "color">' is not assignable to type 'never'.
// const x: never = args;

console.log(args.name, args.version, args.color, args.nickname, args.items);

