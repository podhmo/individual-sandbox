# 手軽にreactを楽しみたい

esbuildなどを使ってbundleせずに手軽に楽しめる様になっているかを確認したい。
（実際のところの開発ではviteの設定をするのが普通そう）

## 00 tsxを生成する

bundler(transpiler)のためのヒントの書き方を忘れがち。やっているうちに覚えたのだけれどきっとすぐに忘れる。

```console
$ deno run -A jsr:@podhmo/glue@0.2.1 init

$ ls
README.md client.tsx main.ts
```

こうやって生成できるようにしておくと便利そう(ところでdeno init --npmのような形式で扱えた方が楽なんじゃないか？)。

実行の仕方はファイルに書かれている。

## 01 APIを呼ぶようなコードも追加してみると良いかもしれない

`GET /readme` みたいなAPIをテキトーに作って呼ぶことにしよう。これ自体は簡単にできる。
（こうなった瞬間にbundleで作られたindex.htmlは無力になるかもしれない。deployが大変になる）

serveで動かすことにする。

```console
$ deno run -A jsr:@podhmo/glue@0.2.1 serve --port 8080 ./main.ts
```

development modeで扱いたい場合には `--development` を付ける。そうするとesm.shに`?dev=`付きでリクエストしてくれる様になる。

## 02 deno.json を追加しておくのを忘れていた

ふつうはdeno.jsonを追加しておくだろうしcompilerOptionsのところにtsx/jsxの扱い方を書いて置くような気がする。
今回の場合はdeno.lockを載せるために書いている。

このままだとserver sideとclient sideの依存が一緒くたになっているので潔癖な人は分けたりするべきなのかもしれない。
