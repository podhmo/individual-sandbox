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
とりあえずそのまま`fetch()`として機能する関数を作ってみる。組み込みの関数はglobalThisを経由して参照すれば良いようだ。

引数や戻り値の形状が変わった場合に備えてちょっとだけトリッキーな形で型を定義している。

```ts
async function fetch(url: string, init?: Parameters<typeof globalThis.fetch>[1]): ReturnType<typeof globalThis.fetch> {
  init = init ?? {};

  // path to url
  if (url.startsWith("/")) {
    url = "https://api.openai.com" + url;
  }

  // set Authorization header iff url is OpenAI API
  const headers = init.headers as Record<string, string> ?? {};
  if (url.startsWith("https://api.openai.com/") && headers["Authorization"] === undefined) {
    headers["Content-Type"] = "application/json";
    headers["Authorization"] = `Bearer ${Deno.env.get("OPENAI_API_KEY")}`;
  }

  if (DEBUG) {
    console.dir({ url, method: init?.method, headers, body: init?.body }, { depth: null });
  }
  const response = await globalThis.fetch(url, { ...init, headers });
  if (DEBUG) {
    if (response.ok) {
      console.dir({ response }, { depth: null }); // 正常系のときにレスポンスを消費したくない
    } else {
      console.dir({ response, text: await response.text() }, { depth: null });
    }
  }

  return response
}
```

これはこれで良いのだけれど、コマンドライン引数でbaseUrl部分に当たる部分が設定できないし、環境変数経由でAPI Keyを取得しているのが気持ち悪い。

## 05 `fetch()` を返すbuilderを作る

> コマンドライン引数でbaseUrl部分に当たる部分が設定できないし、環境変数経由でAPI Keyを取得しているのが気持ち悪い

ここでクラスに...と言っても良いのだけれど、まだまだ関数で頑張れる気がしている。関数を返す関数を書けば良い。
作った関数(ここでは`buildFetchFunctionForOpenAIAPI()`)を使ってAPIを呼び出すためのfetchを作る。

main()関数の中で全部読んでもよい程度の記述量になった(call-chatgpt4.ts)

```ts
    const fetch = buildFetchFunctionForOpenAIAPI({ apiKey: args.apiKey, debug: args.debug });

    const response = await fetch("/v1/chat/completions", {
        method: "POST",
        body: JSON.stringify({
            model: "gpt-4o-mini", // 使用するモデルを指定 ("gpt-4", "gpt-3.5-turbo", など)
            messages: messages,
            max_tokens: 100, // 必要に応じて変更
            temperature: 0.7, // 必要に応じて変更
        }),
    });
```

「axiosはもう不要いや必要」みたいな問題に関してはそもそも各自のアプリケーションの中で仕様として決めてしまってそれ専用のfetchを作れば良いだけな気がする。どうなんだろう？

誤ってglobalThis.fetchを呼ぶ間違いを避けるためにfetchとは異なる名前のほうが良いかもしれない。
ただfetchという名前にしておくと名前を変更せずにいろんなところに持っていけて便利な気もする。

この種の関数を外部から取得されるようなライブラリにしてしまうと、前提条件や処理が追いにくくなり厳しい感じになる気もする。

## 06 それでもAPI Clientが作りたい？

それでもAPI Clientを作りたい気持ちになる場合がある。現状のコードではAPIのエンドポイント部分(コード上では `/v1/chat/completions`)は手書きなのでタイポをしてしまうかもしれない。

## references

- https://chatgpt.com/share/674b0357-e9b4-8001-bb41-04cf6af2f044
- https://platform.openai.com/docs/api-reference/chat/create
- https://platform.openai.com/docs/models#model-endpoint-compatibility
- [ChatGPT-API Error Code 429の解決方法 #ChatGPT - Qiita](https://qiita.com/Keichan_15/items/b1aac09f77c6f8580113)
- https://jsr.io/@std/cli
- https://jsr.io/@std/dotenv