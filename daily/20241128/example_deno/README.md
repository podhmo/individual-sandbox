# mini-jsxの続き

昨日のjsxで遊ぶgistの続き

- https://gist.github.com/podhmo/e8bd70809365ac91e7543de32b77c668

どうやらjsx-runtimeの形式をサポートしてあげると良いみたいだ。概ねkeyの扱いが増えてchildrenをprops経由で受け取るように変わった感じらしい。

## ファイル構造

これをフラットな構造で扱う方法がわからなかった。

```console
$ tree --noreport
.
├── README.md
├── deno.json
├── mini-jsx
│   ├── jsx-runtime.ts
│   └── types.ts
├── use0.tsx
├── use1.js
└── use1.tsx
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

## use1.tsx

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

## 型チェックをどうにか対応させたい

ちなみに現状型チェックは全然だめ。昨日も出てきた `JSX.IntrinsticElements` が存在しないので怒られている。section,h1タグ用の設定がないからのようだ。
これをglobal declare namespaceなしで真面目に作成するのはどうしたら良いんだろう？

```console
$ deno check --unstable-sloppy-imports ./use0.tsx
Check file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241128/example_deno/use0.tsx
error: TS7026 [ERROR]: JSX element implicitly has type 'any' because no interface 'JSX.IntrinsicElements' exists.
    <section className="container">
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241128/example_deno/use0.tsx:4:5

TS7026 [ERROR]: JSX element implicitly has type 'any' because no interface 'JSX.IntrinsicElements' exists.
        <h1>Hello, World!</h1>
        ~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241128/example_deno/use0.tsx:5:9

TS7026 [ERROR]: JSX element implicitly has type 'any' because no interface 'JSX.IntrinsicElements' exists.
        <h1>Hello, World!</h1>
                         ~~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241128/example_deno/use0.tsx:5:26

TS7026 [ERROR]: JSX element implicitly has type 'any' because no interface 'JSX.IntrinsicElements' exists.
    </section>
    ~~~~~~~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241128/example_deno/use0.tsx:9:5

Found 4 errors.
make: *** [Makefile:12: use2] Error 1
```

以下の様な感じでtypes.tsを作ってimportした。classNameしか使っていないのでclassNameしか利用可能にしていない。

```ts
import type * as JSX from "./types.ts"
export type { JSX }

type Node = JSX.Node
```

types.ts

```ts
// JSX namespace
export interface Node {
   // deno-lint-ignore ban-types
   tag: string | Function
   props: Record<string, unknown> & { children: Node[] }
   key: string | undefined
}


export type IntrinsicElements = {
   // [P : string]: {className?: string, children?: Node[]}

   section: {className?: string, children?: Node[]}
   h1: {className?: string, children?: Node[]}
}
```

これでdeno checkは通る様になったが language serverはうまくやってくれないみたい。

## language server側の変更

deno.json側にunstableの情報をもたせれば良いのかもしれない。

```json
{
    "unstable": ["sloppy-imports"]
}
```

こうなってくると、いっそのことcompilerOptions側に設定を寄せてtsxのところでヒントを書かずに済ませられないかと思ったが無理？
書いてみたがなんか怪しい。sloppy-importsとの兼ね合いがなんか怪しい気がしている。そもそもsloppy-importsを外したい。

```json
    "compilerOptions": {
        "jsx": "react-jsx",
        "jsxImportSource": "./mini-jsx",
        "jsxImportSourceTypes": "./mini-jsx/types.ts"
    },
```