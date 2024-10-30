import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args";

// 問題:
// - DefaultKeys をうまく使う方法がわからない
//     - default が指定されている場合にrequiredに含まれていない場合も string になるようにしたい

// リテラル型配列に限定するためのユーティリティ型
type EnsureLiteralArray<T> = T extends ReadonlyArray<string>
    ? string[] extends T // リテラル型でない場合は never[] にする
    ? never[]
    : T
    : never;

type Parsed<StringKeys extends readonly string[], BooleanKeys extends readonly string[], RequiredKeys extends readonly string[], DefaultKeys extends readonly string[]> = {
    [K in StringKeys[number]]: K extends RequiredKeys[number]
    ? string
    : (K extends DefaultKeys[number] ? string : string | undefined);
} & {
    [K in BooleanKeys[number]]: K extends RequiredKeys[number]
    ? boolean
    : (K extends DefaultKeys[number] ? boolean : boolean | undefined);
} & { _: string[] };


// パース関数の定義
function parseArgs<
    StringKeys extends readonly string[],
    BooleanKeys extends readonly string[],
    RequiredKeys extends readonly string[],
>(
    args: string[],
    options: {
        string: EnsureLiteralArray<StringKeys>;
        boolean: EnsureLiteralArray<BooleanKeys>;
        required: EnsureLiteralArray<RequiredKeys[number] extends (StringKeys[number] | BooleanKeys[number]) ? RequiredKeys : never>;
        default?: { [P in StringKeys[number]]: string } | { [P in BooleanKeys[number]]: boolean };
    }
): Parsed<StringKeys, BooleanKeys, RequiredKeys, []> {

    if (args.includes("--help")) {
        console.log(buildHelp(options));
        Deno.exit(0);
    }

    // @ts-ignore options["default"] is conflicted?
    const parsed = originalParseArgs(args, options);

    for (const key of options.required) {
        if (parsed[key] === undefined) {
            console.error(`--${key} is required`);
            Deno.exit(1);
        }
    }
    return parsed as Parsed<StringKeys, BooleanKeys, RequiredKeys, []>; // ここでDefaultKeysを上手く渡したい
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
    string: ["name", "version", "nickname"],
    boolean: ["color"],
    required: ["name", "color"],
    default: { nickname: "------", color: false }, // booleanはdefault値として渡せなくても良い気もする
} as const);

console.dir(args, { depth: null });

// [deno-ts] Type 'Parsed<["name", "version", "nickname"], ["color"], ["name", "color"], []>' is not assignable to type 'never'.
// const x: never = args;
console.log(args.name, args.version, args.color, args.nickname);
// args.nicknameが string | undefined ではなく string になってほしかった
