// 類題3

// 問題
// 次のような関数 I を定義してください。この関数はオブジェクト data と文字列の配列 optionalKeys を受け取ります。optionalKeys で指定されたプロパティは string | undefined 型にし、それ以外のプロパティは string 型にする型定義を作成してください。

// 例
// const result = I({ title: "My Book", author: "John Doe", year: "2023" }, ["year"]);
// // resultの型は { title: string; author: string; year: string | undefined } となるべき

function I<T extends Record<string, string | undefined>, KS extends (keyof T)[]>(obj: T, optionalKeys: KS): { [P in keyof T]: P extends KS[number] ? (T[P] | undefined) : T[P] } {
    return {} as any // 手抜き
}

{
    // [ts] Type '{ title: string; author: string; year: string | undefined; }' is not assignable to type 'never'.
    const result: never = I({ title: "My Book", author: "John Doe", year: "2023" }, ["year"]);
}

{
    // [ts] Type '{ title: string; author: string; year: string | undefined; }' is not assignable to type 'never'.
    const result: never = I({ title: "My Book", author: "John Doe", year: "2023" }, ["year", "another"]);
}