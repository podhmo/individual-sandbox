# denoで読み込めるファイルをhtmlで読み込めるesmに変換したい

自分のHTML/CSSの学習用の環境を作りたい。ファイルは最小限の依存だけをリポジトリにcommitしたい。

## やりたいこと

やりたいことを整理する

- deno用のimport pathをHTMLで使えるように変更する `jsr:<>` -> `<>`
- HTML用のimport mapを生成する `<>` -> `https://esm.sh/jsr/<>`
- それ用のjsを生成してscriptに埋め込む

htmlに一括でbundleされるようなものがほしい？

## 使えそうなもの

- ts-morph (typescriptのcompiler APIのwrapperらしい)
- `deno_*` denoの内部のrust clate (deno_graphなどを使うと楽感はある)
- ↑をwasmに変換したものをtsから使う (deno/emitなどはこの方向)

雑にchatGPTに聞いた。とりあえずts-morphを使った例を試してみる。

https://chatgpt.com/share/6739d60d-3c94-8001-a6e7-3b3c5a29cd68

## Files

- tools.ts -- ts-morthを使ってファイルを変換するだけ
- tools2.ts -- esbuildのpluginを試している

## 00 src_main0.ts

とりあえず https://jsr.io/@std/collections あたりを使って変換してみる。
esm.shを読む感じにしてほしい

https://github.com/esm-dev/esm.sh?tab=readme-ov-file#supported-registries に書いてある

- `jsr:@std/collections@1.0.0/chunk`
- `https://esm.sh/jsr/@std/collections@1.0.0/chunk`

とりあえず、動いたが以下が不足

- HTML用のimport mapが生成できていない
- たぶん.tsそのままだとHTMLから実行できない (.ts -> .jsしないとだめかも？)
- そもそもindex.htmlが生成されない (bundleが必要？)

## 01 src_main1.ts

>[!WARNING]
> めんどくさいのでスキップした

deno.jsonで `deno add jsr:@std/collections` しているときにうまく変換してくれるようにしたい

実は deno graphとかが優秀かもしれない。

https://jsr.io/@deno/graph

## 02 src_main2.ts

bundlingしたい？
考えてみると結局bundleするならesbuildで良いのでは？というわけで昨日の成果を元にesbuildを試してみる。

すごい簡単に作れるかも？

作れた > tools2.ts

考えてみると、esmに変換されるので拡張子は.mjsの方が良いのか。

## 03 src_main3.ts

esbuildにbundlingも任せると普通に相対パスも動くな。。

```js
// hello.ts
import { chunk } from "https://esm.sh/jsr/@std/collections@1.0.9/chunk";
function hello(name) {
  return `Hello, ${name}!`;
}

// src_main3.ts
console.log(hello("world"));
console.log(chunk([1, 2, 3, 4, 5], 2));
```

## 04 src_main4.ts

deno.jsonを見るようにしてみた。一度externalにするとresolveの対象にならない。

欲を言うと、ここでのtree-shakingも実装したい。

https://esm.sh/#docs

> ```
> import { __await, __rest } from "https://esm.sh/tslib"; // 7.3KB
> import { __await, __rest } from "https://esm.sh/tslib?exports=__await,_rest"; // 489B
> ```

## 05 src_main5.tsx

npmも読むようにしてみた。esbuildならtsxも対応しているでしょということでそれっぽい感じに読むようにしてみた。
なんか普通にうまくいっているっぽい。

こんなコードが生成される。

```js
// src_main5.tsx
import { h } from "https://esm.sh/preact@10.24.3";
function Code({ children, language }) {
  return /* @__PURE__ */ h("pre", { ...language ? { language } : {} }, children);
}
var Hello = ({ name }) => {
  return /* @__PURE__ */ h("h1", null, "Hello ", name, "!!");
};
export {
  Code,
  Hello
};
```

そして以下のようなrun5.mjsを書いてdenoで実行してあげていた。

```js
import { renderToString } from "npm:preact-render-to-string";
import { Hello } from "./dst_main5.mjs";

// <h1>Hello World !!</h1>
console.log(renderToString(Hello({ name: "World" })));
```

run5.htmlというファイルを作り、ここからdst5_main.mjsを読み込んでみたが、このhtmlに書くpreact自体のバージョンを読み込もうとするのが難しいかもしれない。
いっそのことrenderもdst5_main.mjsに含んでしまったほうが良いかも？

こんな感じになる。

```html
    <script type="module">
        import { render } from "https://esm.sh/preact@10.24.3" // ここを揃える必要があるのが難しいかも？
        import { Hello } from './dst_main5.mjs';
    
        render(Hello({name: "World"}), document.querySelector('#app'));
    </script>
```