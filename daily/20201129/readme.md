## aws lambda goをdeploy

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/golang-package.html
- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/golang-handler.html

runtime は go1.x

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/lambda-runtimes.html

### 取れる情報

https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/golang-context.html

example_lambda/02pp/main.go

- x-amzn-trace-id
- lambdacontext.key

  - AwsRequestID
  - InvokedFunctionArn
  - Identity
    - CognitoIdentityID
    - CognitoIdentityPoolID
  - ClientContext
    - Client
      - InstallationID
      - AppTitle
      - AppVersionCode
      - AppPackageName
    - Env
    - Custom

と言うかここを見れば良い

```console
go doc -all github.com/aws/aws-lambda-go/lambdacontext
```

### 利用できる環境変数

```go
func init() {
	LogGroupName = os.Getenv("AWS_LAMBDA_LOG_GROUP_NAME")
	LogStreamName = os.Getenv("AWS_LAMBDA_LOG_STREAM_NAME")
	FunctionName = os.Getenv("AWS_LAMBDA_FUNCTION_NAME")
	if limit, err := strconv.Atoi(os.Getenv("AWS_LAMBDA_FUNCTION_MEMORY_SIZE")); err != nil {
		MemoryLimitInMB = 0
	} else {
		MemoryLimitInMB = limit
	}
	FunctionVersion = os.Getenv("AWS_LAMBDA_FUNCTION_VERSION")
}
```

### todo

- memoryのちょうど良いサイズが分からない
- invokeとlogがスムーズにできない

```
--invocation-type (string)

    Choose from the following options.

        RequestResponse (default) - Invoke the function synchronously. Keep the connection open until the function returns a response or times out. The API response includes the function response and additional data.
        Event - Invoke the function asynchronously. Send events that fail multiple times to the function's dead-letter queue (if it's configured). The API response only includes a status code.
        DryRun - Validate parameter values and verify that the user or role has permission to invoke the function.

    Possible values:

        Event
        RequestResponse
        DryRun
```

## inflexible

- flag.Valueを定義してみる
- validatorを追加してみる

### もう少し整理

- validator

  - validatorの表示をきれいに
  - validatorの内容をopenAPI docとして表現
  - validatorに条件を加えられるようにする方法を考えてみる (optional)

- openAPI doc

  - openAPI docを表示する例を追加する
  - openAPI docやvalidatorにenumを追加してみる

- graceful stopの追加
- コード生成

  - handler部分をコード生成で作るように書き換える

- lifetime

  - startup eventを用意する
  - registryのちょうど良い立ち位置を見つける

- 実際にdbを使ったsampleを追加してみる
- lambdaでの実行を試してみる



