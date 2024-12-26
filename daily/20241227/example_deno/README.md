# esm.shを利用した htmlを生成してみる

## preactを使った例を試す hello

以下の例を使ってやってみる。

- https://preactjs.com/tutorial/01-vdom/

作成したCLIを使って動かせるか試したい。

- https://github.com/podhmo/deno-glue

```console
$ deno run -A jsr:@podhmo/glue/bundle --output-style html --html-id app 00client.tsx > 00.html
```

preactはどうやらvdomを注入するのであって置き換えるわけではないらしい。

```jsx
/** @jsxRuntime automatic */
/** @jsxImportSource npm:preact */
/** @jsxImportSourceType npm:@types/preact */

import { render } from "npm:preact";

function App() {
    return <p class="big">Hello World!</p>;
}

// main
const root = document.getElementById("app");
if (root !== null) {
    for (const child of Array.from(root.children)) {
        root.removeChild(child);
    }
    render(<App />, root);
}
```

## 01 preactを使った例を試す counter

さすがにhelloを表示するだけならserver sideでdeno runをやれば動くのでもう少しクライアント側のjsxが動くような例を使いたい。signalはカウンターを使ってくれるだろう。

- https://preactjs.com/guide/v10/signals/
- https://www.npmjs.com/package/@preact/signals

signalのパッケージ名は `@preact/signals` なのか。

なんか動かそうとしてみると上手く反応しない。そしてバージョンを調節したら `r is undefined`みたいなエラーで死んでた。developmentモードでよみこむように `?dev` を付けてあげたら動いた。こういうのがあると嫌になるね。。

>[!NOTE]
>developmentモードでビルドできるようなオプションが必要かも？

## 02 細々とした調整がだるい appにしてみる

全部自分で書いてしまったほうが楽な気がする。index.htmlを生成するサーバーを作りたい。
ついでにreactを使う様にしてみるか。deno serveはfetchを持つオブジェクトをdefaultでexportしてるとそれを使う。テキトーに書けばhonoとかがやってくれる。

```console
$ deno serve --port 8080 -A 02app.ts
```

クライアント側のコードはリロードするたびにビルドされる。

APIを呼ぶようなコードを書こうと思ったけれどめんどくさいな。。

## 03 APIを呼ぶコードをパクって動かす

以下のやつをそのままパクって動かす

- https://zenn.dev/laiso/articles/c7eba95ce43feb
- https://github.com/yusukebe/hono-spa-react/tree/main/src

