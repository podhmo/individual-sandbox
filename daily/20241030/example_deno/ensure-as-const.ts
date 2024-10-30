// リテラル型配列に限定するためのユーティリティ型
type EnsureLiteralArray<T> = T extends ReadonlyArray<string>
    ? string[] extends T // リテラル型でない場合は never[] にする
        ? never[]
        : T
    : never;

function createGetValue<T extends readonly string[]>(keys: EnsureLiteralArray<T>): (key: T[number]) => string {
    return (key: T[number]) => {
        return `The value of ${key}`;
    };
}

// 使用例 (as const を指定した場合)
const getValue = createGetValue(["name", "age", "email"] as const);

// 有効なキー
console.log(getValue("name"));  // OK
console.log(getValue("age"));   // OK

// 無効なキー
console.log(getValue("address")); // エラー

// as const を忘れた場合、エラーになる
// [deno-ts] Type 'string' is not assignable to type 'never'.
const getValueWithoutAsConst = createGetValue(["name", "age", "email"]); 
// エラー: 型 'never[]' は 'readonly string[]' のパラメータに割り当てられません

getValueWithoutAsConst("name");
