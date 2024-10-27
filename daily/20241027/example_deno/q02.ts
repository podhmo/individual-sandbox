// 類題2

// 問題
// 関数 H<T, K> を作成してください。この関数はオブジェクト obj を引数として受け取り、obj のうち K で指定されたキーだけを取り出したオブジェクトを返します。ただし、K には存在しないプロパティ名が含まれてはいけません。

// 例
// const result = H({ a: 1, b: "hello", c: true }, ["a", "c"]);
// // resultの型は { a: number; c: boolean } となるべき

function H<T extends Record<string, unknown>, K extends (keyof T)>(obj: T, keys: K[]): { [P in K]: T[P] } {
    return {} as any // 
}

{
    // [ts] Type '{ a: number; c: boolean; }' is not assignable to type 'never'.
    const result: never = H({ a: 1, b: "hello", c: true }, ["a", "c"]);
}
{

    // [ts] Type '"d"' is not assignable to type '"a" | "b" | "c"'.
    // [ts] Type '{ a: number; b: string; c: boolean; }' is not assignable to type 'never'.
    const result: never = H({ a: 1, b: "hello", c: true }, ["a", "c", "d"]);
}


// 別解
function H2<T extends Record<string, unknown>, K extends keyof T>(obj: T, keys: K[]): Pick<T, K> {
    const result = {} as Pick<T, K>
    keys.forEach(key => {
        if (key in obj) {
            result[key] = obj[key]
        }
    })
    return result;
}

{
    // [ts] Type 'Pick<{ a: number; b: string; c: boolean; }, "a" | "c">' is not assignable to type 'never'.
    const result: never = H2({ a: 1, b: "hello", c: true }, ["a", "c"]);
}
{
    // [ts] Type '"d"' is not assignable to type '"a" | "b" | "c"'.
    const result: never = H2({ a: 1, b: "hello", c: true }, ["a", "c", "d"]);
}
