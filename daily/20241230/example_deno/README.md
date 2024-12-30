# denoの `--watch-hmr` オプション

denoの `--watch-hmr` オプションの挙動を把握したかった。
ふつうに自動でreloadしてくれるだけなのかな？

## 00 自分自身の更新チェック

`--watch-hmr` で*.tsファイルの更新は確認できる。でもどうやらhtmlとかの更新確認はしてくれないみたい？

```console
$ deno run -A --watch-hmr 00watch.ts
Watcher Process started.
...
```
## 01 watch others

ファイルを監視する例も用意されていた。

- https://docs.deno.com/examples/watching_files/

Deno.watchFsとdebounceとか使えるんだ。

## 02 ブラウザリロード

テキトーに組み合わせてSSEでブラウザリロードを作ってみる。
手元のファイルが変更されるとブラウザがリロードされる。

以下のようなサーバーを立ち上げる。

- `GET /sse` -- ブラウザリロード用のevent stream
- `GET /` -- 配信対象のhtmlを返す

```console
$ deno run -A 02*.ts

listening on http://127.0.0.1:8080
watching...
debounced: [Object: null prototype] {
  kind: "access",
  paths: [
    "/home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241230/example_deno/./02.html"
  ],
  flag: null
}
...
```