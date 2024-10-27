// 類題5

// 問題
// ある関数 K を作成してください。この関数は、オブジェクト settings と requiredKeys という文字列配列を引数に取ります。requiredKeys に含まれるプロパティ名は boolean 型、その他のプロパティは boolean | undefined 型に制限されます。


// 例
// const result = K({ darkMode: true, notifications: undefined, autoSave: false }, ["darkMode", "autoSave"]);
// // resultの型は { darkMode: boolean; notifications: boolean | undefined; autoSave: boolean } となるべき

function K<T extends Record<string, boolean | undefined>, K extends keyof T>(settings: T, requiredKeys: K[]): { [P in keyof T]: P extends K ? boolean : boolean | undefined } {
    return {} as any
}

{
    // [ts] Type '{ darkMode: boolean; notifications: boolean | undefined; autoSave: boolean; }' is not assignable to type 'never'.
    const result: never = K({ darkMode: true, notifications: undefined, autoSave: false }, ["darkMode", "autoSave"]);
}