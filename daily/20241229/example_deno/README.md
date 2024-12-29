# deno serveもどきが作りたい

簡単な実装で良いので手軽にcacheを有効にしたものが作りたいのだった。

- https://github.com/podhmo/deno-glue/issues/19
- deno serve のように export defaultしたものを読み取るような感じにしたい
- esm.shへのrequestのcacheを利用したい

## dynamic import

dynamic importを呼ぶのにcwdが自身のファイルになっていた。これを防ぐには `file://<absolute path>` という形で記述すれば良いみたい。

- https://docs.deno.com/deploy/api/dynamic-import/

## cacheを追加する

昨日のものをそのまま使うことにしよう。とりあえず動くことを優先にする。

- https://gist.github.com/podhmo/7e485256563c8d0d0539d916b34f47d8

:memo: cacheをpurgeする機能も追加したい

昨日のエラーを結構簡単に発生する？
あと、`[Object.promise].metadata.json` というところにmetadataの記録が保存されるみたいだ。。

一時的に自前で持つことにする？
`ghq get -p denosaurs/cache`

## node:processという謎のリクエスト

これはCORS的な問題だと思うのだけれど他にもエラーが出ている？

```
Access to script at 'node:process' from origin 'http://localhost:8080' has been blocked by CORS policy: 
Cross origin requests are only supported for protocol schemes: chrome, chrome-extension, chrome-untrusted, data, http, https, isolated-app.
```

よく見ると `node:process` というところにアクセスしようとしていてこれは何者？

原因はdenoのfetch()を使っていたからみたい。

## esm.shの仕様っぽい

以下のURLにrequestしたときに302 redirectが発生する

https://esm.sh/stable/react@19/jsx-runtime

そして通常は /stable/react@19.0.0/esnext/jsx-runtime.js などを見に行くが
denoからrequestしたときには https://esm.sh/stable/react@19.0.0/denonext/react.mjs などにリダイレクトされるみたい。User Agentあたりを見ているのかな？

雑に書き換えてみた。とりあえず通る様になった。

## なぜかapplication/jsonと判定されてしまう

次はcontent typeの問題が出てきた。初回アクセス時にはapplication/javascriptと認識されるのにも関わらず二回目はapplication/jsonとして認識されてしまっていた。あー、たぶんこれpreflightなどの仕様に上手く対応できて無さそう。

source mapを返すrequestだけキャッシュしているみたいになっていそう？。

いや、違う。fetchがredirectを自動で対処していてその結果をキャッシュしていたという感じかも。

https://developer.mozilla.org/en-US/docs/Web/API/RequestInit

## metadataに何か残る `[Object.promise].metadata.json`

たぶんどこかでawaitを忘れている。それでpromiseが返ってきている場合がある。

```
$ console ls
[object Promise].metadata.json
client.tsx
main.ts
```

どうやらpath()関数もawaitが必要になったにも関わらずmetapath()でawaitをしていなかったらしい

## とりあえずpushしたが上手くmoduleを読み込めていなかった

どうやら、dynamic import用の設定がだめらしい。

```console
$ deno run -A jsr:@podhmo/glue@0.1.2/serve --port 8080 00app.ts
error: Uncaught (in promise) TypeError: Module not found "https://jsr.io//home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241229/example_deno/00app.ts".
  const m = await import(`../../../../../../${resolved}`);
            ^
```

色々悩んだ。結局resolveの代わりにDeno.realPath()を使えば良いだけだった。
ちなみにpublish時に以下のようなwarningが出る。

```console
$ deno publish
Checking for slow types in the public API...
warning[unanalyzable-dynamic-import]: unable to analyze dynamic import
   --> /home/po/ghq/github.com/podhmo/deno-glue/serve.ts:126:26
    |
126 |   const m = await import(specifier);
    |                          ^^^^^^^^^ the unanalyzable dynamic import
    |

  info: after publishing this package, imports from the local import map / package.json do not work
  info: dynamic imports that can not be analyzed at publish time will not be rewritten automatically
  info: make sure the dynamic import is resolvable at runtime without an import map / package.json

Visit https://jsr.io/auth?code=GKJY-SNCA to authorize publishing of @podhmo/glue
Waiting...
Authorization successful. Authenticated as podhmo
Publishing @podhmo/glue@0.1.3 ...
Successfully published @podhmo/glue@0.1.3
```

## とりあえず動かす

windowsだと `win + ←` と `win + →` で左右にウィンドウに移動してあげると良さそう。そしてHMRが欲しくなってきた。

ところで、pico.cssもnpmで参照できたほうが嬉しいかというとtsx側に置かないので困らないのか。styled componentみたいなjs側で書くようになってくると変わってくるのかもしれない。