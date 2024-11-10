# const type parametersを利用すればliteral unionが渡された時だけを解釈できる？

元々は以下のようなユーティリティ型を経由して`as const`を強制させていた。

```ts
// for enfoce as-const assertion
type EnsureLiteralArray<T> = T extends ReadonlyArray<string> ? string[] extends T // if T is not a literal type, return never[]
    ? never[]
  : T
  : never;
```

const type parametersを知らなかった当初はこれにより型エラーに気づき`as const`を付けることを要求していたのだけれど、const type parametersを使えば良いのかもしれない。

## 実験

ここからは実験

### array

 これはもしかして今まで必要になってきたas const assertionが不要になるのでは?

 いけそう(./const_type_parameters.ts)。

 ```ts
 function use2<const T extends string[]>(xs: T): { xs: T } {
    return { xs: xs }
}
 ```

 気になるのは以前必要になっていたEnforceLiteralArrayみたいなユーティリティも不要になるかも？ということ
 あと、`T[number][]`みたいな謎の作業も不要になっている？

### object

optionsでも上手くいくんだろうか？

この引数がundefinedのときはどうだろう？

### [] or undefined or ["x", "y"]

実際の利用方法はパラメーターがが省略可能になっている。その辺を含めるとAssertLiteralType()は消せなかった。
渡される可能性のあるものはこの省略されたときにwideingが発生しがちだった。

- 省略      - - ["x", "y", "z"], undefined
- 明示的に空  -- ["x", "y", "z"], []
- subset     -- ["x", "y", "z"], ["x"]
- not subset -- ["x", "y", "z"], ["i"]

typeofでparameterを取り出す必要ががあるかと色々調べたが、結局 `(options: {xs?:T})` みたいな引数の`typeof options.xs` は `T | undefined` に常になるだけだったので特に気にする必要はなかった。

### literal unionがいい感じに渡された時だけうまくいく場合を調べた

📝 型レベルの分岐を調べるときには `{<key name>: T}` みたいな形で書いてあげるとわかりやすい

ok,widen1,widen2というタグをつけて調べてる。
ちなみに結果がokの型でも引数の制約に引っかかってエラーになっている（T ⊂ G）

```ts
function X<
    const T extends readonly string[],
    const G extends readonly string[]
>(
    options: { xs?: T extends G ? T : never, ys?: G } // T ⊂ G
): readonly [] extends T ? { widen1: never } : (string extends T[number] ? { widen2: never } : { ok: T[number] }) {
    return options as any
}

const ys = ["x", "y", "z"] as const
let _: never = X({ ys })                       // { widen1: never; }
let _: never = X({ ys, xs: [] })               // { widen1: never; }
let _: never = X({ ys, xs: undefined })        // { widen1: never; }
let _: never = X({ ys, xs: ["x"] })            // { ok: "x"; }
let _: never = X({ ys, xs: ["a", "b"] })       // { ok: "a" | "b"; }
let _: never = X({ ys, xs: ["a", "b", "c"] })  // { ok: "a" | "b" | "c"; }
const xs: string[] = ["x"];
let _: never = X({ ys, xs })                    // { widen2: never; }
```

## 適切に引数の位置でエラーメッセージを出したい

単純に実装するとundefinedやneverに値を代入できないみたいなってしまう。以下の様に定義してあげるとまともなエラーメッセージになる。

```ts
// for extract literal union
type ExtractLiteralUnion<T extends readonly string[]> = readonly [] extends T ? never : (string extends T[number] ? never : T[number]);


function parse<
    const StringKeys extends readonly string[],
    const CollectKeys extends readonly string[],
>(
    options: {
        string?: StringKeys
        collect: ExtractLiteralUnion<CollectKeys> extends ExtractLiteralUnion<StringKeys> ? CollectKeys: ExtractLiteralUnion<StringKeys>[]
    }
): { string: ExtractLiteralUnion<StringKeys>, collect: ExtractLiteralUnion<CollectKeys>, p: typeof options.collect } {
    return options as any;
}

const _options = parse({
    string: ["name", "version",],
    collect: ["name", "x"], // // [deno-ts] Type '"x"' is not assignable to type '"name" | "version"'.
});
```

 # 参考

 - https://speakerdeck.com/nearme_tech/typescript-5-dot-0-const-xing-parametanoshi-idao
