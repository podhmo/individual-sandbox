# denoでgeminiのweb APIをライブラリを使わず呼んでみる

## 00 hello world

とりあえず以下のページにある内容を元にrequestを呼んでみる。

- https://ai.google.dev/gemini-api/docs/text-generation?hl=ja&lang=rest

認証はkeyで済むみたい

```
  curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=${GEMINI_API_KEY}" \
    -H 'Content-Type: application/json' \
    -X POST \
    -d '{
      "contents": [{
        "parts":[{"text": "Write a story about a magic backpack."}]
        }]
       }'
```

それっぽい感じで作った。`gemini-1.5-flash` 以外のpathにアクセスしたい。

## 01 call with another model

> それっぽい感じで作った。`gemini-1.5-flash` 以外のpathにアクセスしたい。

現状のpathを推測する記述の方法だとpathに変数が含まれたものに対応できない。

以下のページ元にとりあえずmodelを変えてリクエストしてみる。

- https://ai.google.dev/gemini-api/docs/models/gemini?hl=ja
- https://ai.google.dev/gemini-api/docs/text-generation?hl=ja&lang=rest

とりあえず、`${model}`という形でpathを保持しておきfetchの中でreplaceすることにしてみた。
`new URL()`を通して変換した結果のURLはurlencodeされている。それ以前のpathの部分でreplaceしてあげた方が自然かも？

- :memo: ストリーミングに対応する必要がある
- :memo: modelをコマンドライン引数から選べるようにする

## 02 select model

ここは単にmodelを外に出しただけ。

```console
$ deno run -A call-gemini2.ts --help
Usage: cli [options]

Options:
  --apiKey     <string> (required) (default: apiKey="AIzaSyBRcBAfQUtb1Gnna58FuGxUqp1FNYa2R0Y")    (env: GEMINI_API_KEY)
  --baseUrl    <string> (default: baseUrl="https://generativelanguage.googleapis.com")
  --model      <string> one of ["gemini-1.5-flash","gemini-1.5-flash-8b","gemini-1.5-pro"], (default: model=gemini-1.5-flash)
  --debug      (default: debug=false)    (env: DEBUG)
  --help       show help

$ deno run -A call-gemini2.ts --model gemini-1.5-flash-8b
```

(全く関係ないけれど `@podhmo/with-help` でRestrictionを使うと渡したoptionのdefinitionが必要になる。これを引数以外のところで使おうとすると補完が効かなくなる。不便)

- :memo: ストリーミングに対応する必要がある
- :memo: 画像を入力として渡せるようにする

## 03 stream support

APIのドキュメントがまた別の箇所に存在していた。

- https://ai.google.dev/api/generate-content?hl=ja#v1beta.models.streamGenerateContent

`alt=sse`を付けて呼び出すとSSEとしてレスポンスを返してくれるようだ。
とりあえず、テキトーにevent-streamをやってくれる用のライブラリを使ってしまう。

- https://jsr.io/@lukeed/fetch-event-stream

残っている作業

- :memo: 画像を入力として渡せるようにする
- :memo: SSEの中身を調べる
- :memo: いままでAPIに渡すパラメーターが同じものだった。

## 04 テキトーにモデルの一覧を表示させてみる (sub command)

サブコマンドを定義しようとすると同じ記述の重複が目立つようになるかもしれない。
あとは、fetchの利用にmodelが必須になっていた部分が牙を剥く感がある（これはmodelが必須というよりはpath変数部分のdefaultが存在しうるという話かも？）。

特に意味はないが以下のことをした

- サブコマンドにすることにした
- json,textのformatを選択できるようにした

## 05 サブコマンドのbase側に共通オプションを持たせてみる (broken)

stopEarlyをつければサブコマンドも定義できはする。
（ヘルプメッセージが出せない。restrictionだけではなくoptionsも持てないとだめ？）

## references
- https://ai.google.dev/gemini-api/docs/api-key?hl=ja
- https://ai.google.dev/gemini-api/docs/text-generation?hl=ja&lang=rest
- https://aistudio.google.com/app/
- https://ai.google.dev/api/generate-content?hl=ja#v1beta.models.streamGenerateContent
- https://jsr.io/@lukeed/fetch-event-stream
