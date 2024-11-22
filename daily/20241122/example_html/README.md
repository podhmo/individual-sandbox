# import mapで利用できる文字列はどんな感じなんだろう？

`npm:react` などはhtmlのimport mapではうまく動いてくれなかった。どのような文字列ならいけるのか？

## hello.html

とりあえずimport map無しで使ってみる。
何かしらのサーバーを建てないと動いてくれないらしい。

## hello2.html

import mapを素直に使ってみる。helloを読めるようにした。

```html
<script type="importmap">
{
    "imports": {
        "hello": "./hello.js"
    }
}
</script>
<script type="module">
    import { hello } from 'hello';
    document.getElementById('text').textContent = hello("world");
</script>
```

## hello3.html

ここからが本題 `@hello`, `$hello` などをやってみる。結構なんでも行けそう。「🤖hello」みたいなものでも大丈夫。
ふつうに`<protocol>:<path>`みたいな形式がだめっぽいな。他のものと混同されやすいからだと思う。

`http://example.com/hello` とかでも指定可能なので単純にそのようなプロトコルが存在しない的なエラーになってしまうみたい。

## hello4.html

`/` で終わるような指定はサブディレクトリを含めてリダイレクトする感じで機能する。
`greeting`ディレクトリをいい感じに読むには以下のようなimport mapを書く。
(ところで `$greeting` を指定したときにはどの様になるか？というと別途書いて挙げる必要がある)

<script type="importmap">
{
    "imports": {
        "$greeting": "./hello.js",
        "$greeting/": "./greeting/"
    }
}
</script>

<script type="module">
    import { hello } from '$greeting';
    import { bye } from "$greeting/bye.js";

    document.getElementById('text').textContent = `${hello("world")}\n${bye("world")}`;
</script>

ディレクトリのファイル構造は以下の様な状態。

```console
$ find . -name "*.js"
./greeting/bye.js
./hello.js
```

## 文脈

- https://zenn.dev/link/comments/05f174d05ba9bd
- https://github.com/WICG/import-maps?tab=readme-ov-file#specifier-remapping-examples