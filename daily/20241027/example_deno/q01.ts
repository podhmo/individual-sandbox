// 類題1

// 問題
// ある関数 G を作成してください。この関数は、以下の2つの引数を受け取ります：

//     オブジェクト型 obj。このオブジェクトのプロパティは string | number | undefined のいずれかの型を持つ。
//     文字列の配列 keys。keys の中に含まれるプロパティ名に対しては戻り値の型で string に絞り込み、それ以外のプロパティは元の型のままとしてください

// 例
// const result = G({ id: 1, name: "Alice", age: undefined }, ["name"]);
// // resultの型は { id: number; name: string; age: undefined } となるべき

function G<T extends Record<string, string | number | undefined>, K extends (keyof T)>(obj: T, keys: K[]): { [P in keyof T]: P extends K ? (T[P] extends undefined ? (string | undefined) : string) : T[P] } {
    return {} as any // 型だけ知りたいので実装はテキトー
}

{
    // [ts] Type '{ id: number; name: string; age: undefined; }' is not assignable to type 'never'.
    const result: never = G({ id: 1, name: "Alice", age: undefined }, ["name"]);
}
{
    // [ts] Type '{ id: string; name: string; age: string | undefined; }' is not assignable to type 'never'.
    const result: never = G({ id: 1, name: "Alice", age: undefined }, ["id", "age"]);
}
