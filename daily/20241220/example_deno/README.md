# denoでviteを使おうとしてみる

## npm:create-vite@latest

手順は以下のページにそのまま載っている

- https://docs.deno.com/runtime/fundamentals/web_dev/#vite

```console
$ deno run -A npm:create-vite@latest
$ cd my-vite-project
$ deno install
$ deno task dev
# または deno task build
```

index.html -> src/App.tsx という参照を観てviteはHMRする。
ちなみに、`deno task dev` して開いた箇所で `o + enter` とかするとブラウザが開いて便利。

deno.config.ts 自体はreactのpluginを有効にしているだけ。

(これで動くのだけれど、余分なものが多過ぎるきがする)


## 直接viteを使う

アプリを作る分にはこのような構成でも良いけれど可能なら1ファイルですませたい。
viteを動かす分にはvite.config.tsとindex.htmlは必要になる。これをオプションで済ませられないか？

```console
# $ deno run -A npm:vite --cors --config vite.config.ts --logLevel info --debug --mode dev
$ deno run -A npm:vite --cors --logLevel info --debug --mode dev
```

## react plugin

index.htmlから経由するのでvite react pluginを有効にするconfigが必要になる。
https://github.com/vitejs/vite-plugin-react/blob/main/packages/plugin-react/README.md

まだ上手くいっていないこと

- 各種型の情報を取ってくる方法
- import部分の取り扱いをdeno仕様にする方法 `npm:react` ではなく `react` で取得できないと困る

