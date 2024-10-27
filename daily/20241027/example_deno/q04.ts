// 類題4

// 問題
// 関数 J を作成してください。この関数はオブジェクト obj と文字列の配列 keys を引数に取ります。keys に含まれるプロパティは数値型 number に変換し、それ以外のプロパティは obj の型をそのまま保持してください。

// 例
// const result = J({ id: "001", age: "20", name: "Alice" }, ["age"]);
// // resultの型は { id: string; age: number; name: string } となるべき

function J<T extends Record<string,unknown>, K extends keyof T>(obj: T, keys: K[]): {[P in keyof T]: P extends K ? number : T[P]}{
    return {} as any // 手抜き
}

{
    // [ts] Type '{ id: string; age: number; name: string; }' is not assignable to type 'never'.
    const result : never = J({ id: "001", age: "20", name: "Alice" }, ["age"]);
}