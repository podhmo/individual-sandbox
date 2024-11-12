# JSXを弄りたい。素直にreactを利用してみる

素直にreactを使ってみる。のちにpreactも。

>[!NOTE]
>特にdeno.jsonを作らなくてもdeno.lockは作られるみたい。それはそうかも？

## render-to-string.tsx

テキトーに.tsxを作ってdenoで実行してみる。`npm:react`などをimportしておけば良さそうだった。
tsconfig.jsonの設定を全くせずにすることはできるんだろうか？

## render-to-string2.tsx

同様の形にするが `npm:react-jsx` を使う感じでやろうとしてみた。
なんだかうまくいかない。以下のようなエラーが出る。

> error: Relative import path "react/jsx-runtime" not prefixed with / or ./ or ../

もしかして `/** @jsxImportSource react */` はうまく動かなくて `// @jsxImportSource react` が動く？

## render-to-string3.tsx 

preactを使ってみる。deno.jsonにcompilerOptionsを指定することはできる。

```json
{
    "compilerOptions": {
        "jsx": "react",
        "jsxFactory": "h",
        "jsxFragmentFactory": "Fragment"
    }
}
```

これを使わずに指定する方法が分かっていない。何かアノテーションコメントが機能していない

> error: Uncaught (in promise) ReferenceError: React is not defined
>         <App>
>         ^

どうやら `/** @jsx h */` だけ指定すれば良いみたい。正確に言うとimportより上に書けば良いらしい。


### 参考

- https://docs.deno.com/runtime/reference/jsx/
- https://github.com/evanw/esbuild/issues/138
- https://deno.com/blog/fresh-1.1#automatic-jsx