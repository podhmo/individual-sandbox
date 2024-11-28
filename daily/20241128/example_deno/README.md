# mini-jsxの続き

昨日のjsxで遊ぶgistの続き

- https://gist.github.com/podhmo/e8bd70809365ac91e7543de32b77c668

どうやらjsx-runtimeの形式をサポートしてあげると良いみたいだ。概ねkeyの扱いが増えてchildrenをprops経由で受け取るように変わった感じらしい。

## ファイル構造

これをフラットな構造で扱う方法がわからなかった。

```console
$ tree --noreport
.
├── Makefile
├── README.md
├── mini-jsx
│   └── jsx-runtime.ts
└── use0.tsx
```

## use0.tsx

以下の様にしてあげると、jsxでのfactory関数のimport先が変わる。自動的に `./mini-jsx/jsx-runtime` を参照してimportする形に変わるようだ。
そして、この方法の場合にdenoの明示的に拡張子を付ける方法と衝突するから `--unstable-sloppy-imports` が必要になる。

```ts
/** @jsxImportSource ./mini-jsx */

const element = (
    <section className="container">
        <h1>Hello, World!</h1>
        <>
            This is a fragment!
        </>
    </section>
)

console.dir(element, { depth: null })
```

実行結果はそれなりに見れる様になる(そういえばclassNameである必要があるのはreactだけなんだろうか？)。

```console
$ deno run -A --unstable-sloppy-imports ./use0.tsx
{
  tag: "section",
  props: {
    className: "container",
    children: [
      {
        tag: "h1",
        props: { children: "Hello, World!" },
        key: undefined
      },
      {
        tag: [Function: Fragment],
        props: { children: "This is a fragment!" },
        key: undefined
      }
    ]
  },
  key: undefined
}
```

### use1.tsx

ちなみに、use0.tsxのままesbuild経由で変換しようとすると不足するらしい。以下の様な警告が出る。

```console
▲ [WARNING] The JSX import source cannot be set without also enabling React's "automatic" JSX transform [unsupported-jsx-comment]

    use0.tsx:1:21:
      1 │ /** @jsxImportSource ./mini-jsx */
        ╵                      ~~~~~~~~~~

  You can enable React's "automatic" JSX transform for this file by using a "@jsxRuntime automatic"
  comment.

1 warning
```

そんなわけで `/** @jsxRuntime automatic */` を付ける。これがuse1.tsx。

```diff
--- use0.tsx
+++ use1.tsx
@@ -1,4 +1,5 @@
 /** @jsxImportSource ./mini-jsx */
+/** @jsxRuntime automatic */
 
 const element = (
     <section className="container">
```

それっぽい感じになった。`--bundle`していないのでそのままmini-jsxのimportが残る。

```console
$ esbuild use1.tsx
import { Fragment, jsx, jsxs } from "./mini-jsx/jsx-runtime";
const element = /* @__PURE__ */ jsxs("section", { className: "container", children: [
  /* @__PURE__ */ jsx("h1", { children: "Hello, World!" }),
  /* @__PURE__ */ jsx(Fragment, { children: "This is a fragment!" })
] });
console.dir(element, { depth: null });
```



## 