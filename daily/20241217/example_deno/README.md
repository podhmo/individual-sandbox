# 手軽にweb apiサーバーを立ててブラウザから呼びたい

## 00 とりあえず依存なしでhello worldを作る

とりあえず依存なしでhello worldを呼ぶ。
Deno.serveを渡してあげるとそれがserverになるみたい。

- https://docs.deno.com/runtime/fundamentals/http_server/

どうやらoverloadがたくさん設定されていてかなりだるい。あと今の段階ではpath変数を扱う方法が分かっていない。

## 01 deno serveを使ってみる

たしかdenoはserve的なコマンドがあった記憶。

- https://docs.deno.com/runtime/reference/cli/serve/

以下のような構造のコードがあれば良いみたい。

```ts
export default {
  fetch: (Request) => { ... }
}
```

>[!NOTE]
> `deno serve <>.ts --port 8080` のように書くと作ったスクリプトに `--port` の値が渡されてしまうので注意。
> deno serveに渡すには `deno serve --port 8080 <>.ts` みたいに書く必要がある。


まぁたぶんdeno serveの価値というのはそういうパッケージを作っておきつつmain関数を作らず呼べるということに意味があるのだろう。

## 02 path変数を導入してroutingする。

path変数を利用するにはURLPatternを使うとかが無難なんだろうか？雑な正規表現でのrouterということになる。

examplesにrouterの例が存在していた。

- https://docs.deno.com/examples/http_server_routing/

こんな感じで呼べるようになった。

```console
$ http -b :8080/hello/world
{
    "message": "Hello, world!"
}
$ http -b :8080/hello/deno
{
    "message": "Hello, deno!"
}
```

## 03 htmlのindex pageを作りapiを呼ぶ

とりあえずcontent-typeが指定されていない場合にはHTMLを返すようにした。
割と文字列直書きの素直な実装だけれど、ここでindex.htmlにtsxが書けると嬉しい。

🐾 一応デモ用のコードという意味ではここで完成している。


## 04 jsxを利用する

main.tsxでindex.htmlを表示する。tsxとdeno serveを混ぜるのは可能なんだろうか？
とりあえずテキトーにreactあたりで済ませておくか。一足飛びに進むのではなくjsxから文字列を生成して標準出力に書き出すスクリプトを書いてみる。

ヒントを有効にする書き方を忘れてしまった。記憶が確かならbabelのpluginだった記憶。

- https://babeljs.io/docs/babel-plugin-transform-react-jsx
- https://docs.deno.com/runtime/reference/jsx/

`@jsxImportSource`で良いのだっけ？型の取得は？ `@jsxImportSourceTypes` か。
とりあえずhello world的に標準出力にそのまま出力してみる。

📝 Deno.exit()を呼ばないとprocessが生きている感じがした（なぞ）

## 05 jsxをdeno serveで呼ぶ (途中)

初回の読み込みをreactでやるのはできたけれど、bundleしたものをdeno serveで受け取る事はできない？
この辺になってくると真面目にesbuild的なものでアプリをbundleしてそれを呼ぶようにしないとだめ？
(RSCとかを標準でサポートしていない限り無理か...)

うーん、この辺になってくるとhonoとかを使った方が楽かもしれない？

## references

- https://docs.deno.com/runtime/reference/cli/serve/
- https://docs.deno.com/runtime/fundamentals/http_server/
- https://babeljs.io/docs/babel-plugin-transform-react-jsx
- https://docs.deno.com/runtime/reference/jsx/