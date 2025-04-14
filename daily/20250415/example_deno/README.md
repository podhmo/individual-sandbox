# deno streams APIを使ってみたい

- ReadableStream
  - ReadableStream.from() で array,mapやasync iteratorから変換
  - for awaitでループ出来る
- TransformStream
  - pipeThrough() で接続
- WritableStream
  - pipeTo() で接続

references

- https://docs.deno.com/api/web/streams
- https://jsr.io/@std/streams


## 00 とりあえずファイル入出力

行区切りのstreamが手に入ればうれしい。
stdoutをcloseしないでpipeTo()に渡す方法はないものか？(追記: preventCloseオプションを使えば良いことが分かった)


## 01 自作でstreamを作ってみる

それぞれのstreamのconstructorを自分で使って作ってみる。
