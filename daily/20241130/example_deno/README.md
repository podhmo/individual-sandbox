# fetch()のwrapperを作れば十分？

なんとなくChatGPTのAPIを利用するコードから整形していき良さそうなコードを探してみる。

## 00 ChatGPTにコードを提案してもらう

以下の様なプロンプトで提案してもらった。諸々古い部分もあるかもだけれどそれっぽいコード。

> chatGPTのcompletionsのAPIを呼ぶtypescriptのコードを書きたいです。ライブラリは使わずfetchでリクエストしてください。

修正せず回答で返って来たコードはcall-chatgpt0.tsとして作る。

気になるポイントはいくつかある

- 機能
    - APIKeyを環境変数から受け取りたい
- debug
    - APIKeyが設定されていないときのエラー
    - Requestに失敗したときのエラー
    - 429的なResponseが返ってきたときのエラー
    - Responseが期待した通りじゃないときのエラー
- コード
    - ベタ書きをやめたい
    - Errorの対応が重複してそうなのでだるい（console.errorが多すぎる）

### debugしづらい

debugしづらいことについてはもう少し説明を追加しておく。
そのまま実行するとおかしな結果が出るのは問題ない。ただしエラーが以下の様な感じになる。こういうのをやめたい。

```
Failed to fetch chat completion: Error: Error: 401 - Unauthorized
```

grantがexpiredしてたアカウントで実行すると以下の様になる。理由がわからなくて困る。この辺がデバッグしづらいコードになっている。

```
Failed to fetch chat completion: Error: Error: 404 - Not Found
```
modelの指定が古かったので`gpt-4o-mini`に変える。これも理由がわからない。

```
Failed to fetch chat completion: Error: Error: 429 - Too Many Requests
```

このときresponseが表示されていないとデバッグがしづらい。実際このときのエラーの理由はresponseには含まれている。

```json
{
    "error": {
        "message": "You exceeded your current quota, please check your plan and billing details. For more information on this error, read the docs: https://platform.openai.com/docs/guides/error-codes/api-errors.",
        "type": "insufficient_quota",
        "param": null,
        "code": "insufficient_quota"
    }
}
```

せめてdebugオプションを有効にしたら生のrequest/responseが見られるようにされてると嬉しい。
(print debugもdebuggerを起動することもしたくない)

## 01 API Keyを直接コードに記述するのを止める

とりあえず、API Keyを直接コードに記述するのをやめたい。`@std/cli/parse-args` や `@std/dotenv` を使うと便利。
`@std/dotenv/load` をimportすると自動的に.envを読み込んでくれるらしい。

そんな感じで変更したものをcall-chat-gpt1.tsとして作る。個人的には `@std/cli/parse-args` のwrapperを自作したのでこちらを使う。
実行結果は変わらずヘルプメッセージが以下の様な感じになる(ヘルプメッセージでキーが露出するのは微妙かも？)。

```console
$ deno run -A call-catgpt1.ts
Usage: cli [options]

Options:
  --apiKey    <string> (required) (default: apiKey="sk-proj-...")    (env: OPENAI_API_KEY)
  --help      show help
```

このときの.envは以下の様な感じ(もちろん書き換えてる)。

```
OPENAI_API_KEY=sk-proj-...
```

## 02 request/responseをトレースしたい

debugしづらいのでデバッグオプションを作る。ついでに環境変数でもdebugができると良い。
`console.dir()`で雑に出力した。便利。ただstdoutに出力するのが不便かもしれない。

トレースするときには`response.text()`を含める必要がある。
responseを一度消費してしまうと2回目の消費ができなくなったりするので実行時エラーが起きるかもしれない。
まぁ今回は気にしないことにする。

(ついでにheaderを出力するときにマスキングしたくなるかもしれない)

```
{
  url: "https://api.openai.com/v1/chat/completions",
  headers: {
    "Content-Type": "application/json",
    Authorization: "Bearer sk-proj-..."
  },
  body: {
    model: "gpt-4o-mini",
    messages: [
      { role: "system", content: "You are a helpful assistant." },
      { role: "user", content: "What is the capital of France?" }
    ],
    max_tokens: 100,
    temperature: 0.7
  }
}
{
  response: Response {
    body: ReadableStream { locked: true },
    bodyUsed: true,
    headers: Headers {
...
    },
    ok: false,
    redirected: false,
    status: 429,
    statusText: "Too Many Requests",
    url: "https://api.openai.com/v1/chat/completions"
  },
  text: "{\n" +
    '    "error": {\n' +
    '        "message": "You exceeded your current quota, please check your plan and billing details. For more information on this error, read the docs: https://platform.openai.com/docs/guides/error-codes/api-errors.",\n' +
    '        "type": "insufficient_quota",\n' +
    '        "param": null,\n' +
    '        "code": "insufficient_quota"\n' +
    "    }\n" +
    "}\n"
}
```

## 04 `fetch()`を分離したい

ここからはリファクタリング。個別に定義されているfetch部分をいい感じに分離して置きたい。APIを呼ぶのが一度きりとは限らないので。
大体の場合にここでAPIClientのクラスを作る感じになるけれど、実際のところクラスは不要かもしれないというのが今回の記事の本題だった（なんと今までは前座だっ。た）。

例えば、組み込みの`fetch()`をラップした自分用の`fetch()`を作れば良いだけかもしれない。



## references

- https://chatgpt.com/share/674b0357-e9b4-8001-bb41-04cf6af2f044
- https://platform.openai.com/docs/api-reference/chat/create
- https://platform.openai.com/docs/models#model-endpoint-compatibility
- [ChatGPT-API Error Code 429の解決方法 #ChatGPT - Qiita](https://qiita.com/Keichan_15/items/b1aac09f77c6f8580113)
- https://jsr.io/@std/cli
- https://jsr.io/@std/dotenv