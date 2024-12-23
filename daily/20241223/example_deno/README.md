# deno + hono + reactでのCSRのコードを動かす

以前は時間切れで失敗していた。

- [denoでviteを使おうとしてみる（未完）](https://gist.github.com/podhmo/cb9ac43bf2af99d32112c58c3267276a)
- [honoをdenoから使ってみる](https://gist.github.com/podhmo/b2f68ae38a21485da77cdf152e73a92a)

前者ではviteを上手く機能させることができなかった。後者ではどうもCSRが上手く動いていなかったようだった。
後者についてはおそらくサーバー側のコードでhonoの方のjsxを使ってしまっていたからなのではないかと思う。

## やりたいことを整理

色々作業をする前にやりたいことを整理してみる。

- 通常のCSRについて考えてみる
    - CSRの場合はscriptタグで読み込みブラウザ上でjsxが動いてもらう必要がある
    - jsxはjsに変換されなくてはいけない
    - 作ったアプリケーションはDOM上にマウントされなくてはならない(通常はファイルを分割する)。
        - viteで考えるなら(index.html, main.jsx -> main.js, app.jsx程度にはわかれる)
    - scriptタグ側で配信されるならファイル配信的な機能を必要とする
1ファイルでCSRの動作を確認する
    - 最低要件を考えてみる
        - nodeではなくdenoで動かす
        - scriptタグは埋めこみで良い
        - htmlは直書きで良い
        - ライブラリはCDNから読み込むので良い(esm.sh)
        - web APIとの通信は無くて良い。
        - 状態を持つ機能がクライアント側で動いてほしい(e.g. Counter)
        - (ここからのバリエーションが増えていく)
    - オプショナルな要件を考えてみる
        - サーバー側から返すレスポンスをjsxで定義する
        - クライアント側で動くスクリプトで型推論が動くようにする
        - 分割したファイルに対応する
        - web APIを定義してクライアント側から呼びだす
        - ルーティングができるようになる
    - DX的な快適さを手にいれる
        - HMRが機能するようにする(viteとの親和性)
        - 本番用のビルドと開発用のビルドが分かれている(開発用のビルドではローカルのキャッシュを使うようにする)
        - npmの依存を排除する        
        
別に困らないのでhonoは使う

## 00app.ts server HTML, no js

ただただhtmlを返すようなコードを書く。今回はHTMLも最低限度にする

```console
# $ deno serve --port 8080 00app.ts

$ http -b :8080
<h1>hello world</h1>
```

## 01app.ts server HTML, js, hello world

クライアント側でのコードを書く

https://ja.react.dev/learn/installation

```js
function Greeting({ name }) {
  return <h1>Hello, {name}</h1>;
}

export default function App() {
  return <Greeting name="world" />
}
```

https://ja.react.dev/reference/react/StrictMode

```js
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';

const root = createRoot(document.getElementById('root'));
root.render(
  <StrictMode>
    <App />
  </StrictMode>
);
```

これらをわけずに書く。

とりあえずすべてを直書きで済ませることにする。jsxからjsの変換も何もかも存在しない形で書く。
importmapをそれっぽく動かすようにする。

## 02app.ts server HTML, js, counter

カウンターを用意する。こちらも手動で書いてみる。書けないことはないけれどとてもつらい。

- jsxではなく変換後のjsを手書き
- importもコードも補完が一切効かない

無視していたけれど、サーバー側のコードも辛い。

## 03codegen

一旦、jsxを素直にjsに変換するコードを書く。これをやって完成としてしまいがちだった。

- PUREの位置がおかしい気がする？
- `export {  App as default }; ` とか残ってて大丈夫？

## 04app.tsx server jsx, jsx, counter

まだ既存のもののコードを上手く取り出せていない。
ここでコピペして動かすことはできる。配信側をjsxでやろうとすると難しいということか。

以下の様なことがしたいがバグってしまっている。

- server side jsx (hono)
- client side jsx (react)

あとはimportをトップレベルでしかつかえないので１ファイルに纏めることができない。
やるんなら取り出して変換する処理をライブラリの関数として呼び出すという感じにしないとダメそう。

それでも以下の様な感じになりそう

- 04app.tsx
- 04client.main.tsx -> 04client.main.js に変換

そして生成したコードを `dangerouslySetInnerHTML` でscriptタグとして注入する感じに行けばできそう？(1ファイルは無理そう？)

- https://hono-ja.pages.dev/docs/guides/jsx#%E7%94%9F-html-%E3%81%AE%E6%8C%BF%E5%85%A5

とはいえ、viteと同様にベースのindex.htmlがあれば十分では？

## 04 スクリプトタグ部分を変換する

とりあえずesbuildのpluginで変換してみる

- npm:で関連している部分を変換する必要がある
- esbuildのpluginを利用した関数を定義したい
    - ファイル名しか渡せない
    - deno.json, deno.lockの中を覗いて変換をしてみることにする
        - あればそれをみて変換とかで良いか
- どうやら自分自身のresolveを呼ばないみたい
    - bundle: trueを忘れてたみたい

03で書いたスクリプトをそのまま変換してみる。

## 05app.tsx server jsx, jsx, counter

hono側ではhonoのjsxを利用して実行時にesbuildを動かすことにしてとりあえず無理やり動くようにはなった。
ここの部分は結局変数で持ってしまっている。

> それでも以下の様な感じになりそう
> 
> - 04app.tsx
> - 04client.main.tsx -> 04client.main.js に変換

(out of context, deno runとdeno serveで分岐させたくなったりもした)

## references

- https://ja.react.dev/learn/installation
- https://ja.react.dev/reference/react/StrictMode
- https://hono-ja.pages.dev/docs/guides/jsx#%E7%94%9F-html-%E3%81%AE%E6%8C%BF%E5%85%A5

