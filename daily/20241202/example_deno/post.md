# こちらはファイルからの入力

標準入力から読み込んでいい感じに投稿されているかを見る。
考えてみると文字数制限に対応していないので長すぎる文章を投稿しようとしたときに無理になるかも？

プロセス置換でも動くのだっけ？ (だめだった)

```console
$ deno run -A main.ts --content <(cat post.md)
```

`file:<file name>` だったらファイルということにする？

```console
$ deno run -A main.ts --content file:post.md
```
