/** 第一引数の型の範囲を制限したい */

// 元の関数型
type OriginalFunction = (arg: string, num: number) => boolean;

// 第1引数がリテラル型で絞り込まれた関数型を作成する型
type NarrowFirstArgument<
    // deno-lint-ignore no-explicit-any
    F extends (arg: string, ...args: any[]) => unknown, // 引数が文字列を受け取る任意の関数型
    T extends string // 絞り込みに使用するリテラル型
> = (arg: T, ...args: Parameters<F> extends [unknown, ...infer Rest] ? Rest : never) => ReturnType<F>;

// リテラル型 "a" | "b" | "c" に絞り込む
type NarrowedFunction = NarrowFirstArgument<OriginalFunction, "a" | "b" | "c">;

const originalFunction: OriginalFunction = (arg, num) => arg.length === num;

// 実際に使用してみる
{
    const f = originalFunction;

    f("a", 1); // OK
    f("b", 2); // OK
    f("d", 3); // OK (エラーにならない)
}

// 絞り込んだ関数を定義
{
    const f = originalFunction as NarrowedFunction

    f("a", 1); // OK
    f("b", 2); // OK
    // f("d", 3); // エラー: Argument of type '"d"' is not assignable to parameter of type '"a" | "b" | "c"'.
}
