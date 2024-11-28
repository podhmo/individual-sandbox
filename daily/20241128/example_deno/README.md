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

ちなみに、esbuild経由で変換しようとするとこれだけだと不足するらしい。
