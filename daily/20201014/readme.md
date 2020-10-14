## go API client

真面目にstripe goの実装を整理してみる

- 生成部分

  - BackendConfig()から生成
  - Backendというinterfaceがある
  - 実際にはBackendImplmentation

```go
type Backend interface {
	Call(method, path, key string, params ParamsContainer, v interface{}) error
	CallRaw(method, path, key string, body *form.Values, params *Params, v interface{}) error
	CallMultipart(method, path, key, boundary string, body *bytes.Buffer, params *Params, v interface{}) error
	SetMaxNetworkRetries(maxNetworkRetries int)
}
```

呼び出し関係

- BackendImplementation.Call()

  - BackendImplementation.CallRaw()

    - BackendImplementation.NewRequest()
    - BackendImplementation.Do() #

      - BackendImplementation.ResponseToError()
      - BackendImplementation.shouldRetry()
      - BackendImplementation.sleepTime()

- BackendImplementation.CallMultipart()

  - BackendImplementation.NewRequest()
  - BackendImplementation.Do() #

やっぱりそういう感じになるよね。wrap部分は書くかは別として。

- NewRequest()

  - pathを受け取ってURLに
  - Authorization headerを追加
  - content typeを追加
  - APIのversionを追加
  - paramsがあった場合には、Idempotency-keyを追加 (retryのため)

- Do()

  - requestingのlogを追加 (method, req.URL.Host, req.URL.Path)
  - (telemetryが有効なら) request.metricsBufferから取り出してheaderに追加

    - metricsとはrequestIDとrequestDurationMS

  - retry用のloop

    - bytes.Buffer()から新しいbodyを繰り返し作る
    - HTTPClientのDo()を呼ぶ
    - 成功したらlogを追加 (requestDuration, retry)
    - errがnilだったらioutil.ReadAll()で全部読み込む。ここでClose()してしまう
    - errあった場合

      - failedのログを出すだけ

    - statusが400以上

      - ResponseToErrorでエラーを作る
      - 402のときだけinfo。ソレ以外はエラーログ (statusCode, err)

- ResponseToError()

  - bodyから文字列を取り出す
  - このときに関連するJSONからerrになりそうなものを引っこ抜く。なかったらerrors.New()
  - raw.E.Typeで分岐

    - ErrTypeAPI -> APIError
    - ErrTypeAPIConnection -> APIConnectionError
    - ErrTypeAuthentication -> AuthenticationError
    - ErrTypeCard -> CardError
    - ErrTypeInvalidRequest -> InvalidRequestError
    - ErrTypePermission -> PermissionError
    - ErrTypeRateLimit -> RateLimitError

- shouldRetry()

  - MaxNetworkRetries を超えていたらfalse
  - stripeErr以外だったらtrue
  - StripeShouldRetryヘッダーがtrueならtrue (falseならfalse)
  - 409のconflictだったらtrue
  - 428 too many request

    - ErrorCodeがLockTimeoutだったらtrue

  - 5xxでかつPOSTでなければtrue
  - 503ならtrue
  - それら以外はfalse

- sleepTime()

  - networkRetriesSleepがfalseなら0
  - waittime = min(delay + retryDelay * (retry ^ 2), maxRetryDelay)
  - jitter = randInt63(1/4 * waittime)
  - actual wait time = max(waitTime - jitter, minRetryDelay)

## go 自分自身を生成してみたかった

- reflect.StructOf()で自己参照は無理
- なんかいじっていたらバグを見つけてしまった
- ついでに、stripe-go辺りのclientの引数で動くか試してみた


