## 何をしようかな？

- reflect-openapi
- rcmp
- sqs wrapper
- tenuki, go-webtest, snapshot testのまーじ
- いい感じにweb APIのexamplesを作ってみる
- api gatewayを触ってみる
- 静的解析/コード生成用のやつを覗いてみる

## reflect-openapi

直近で少なくとも欲しい機能

- default statusの変更
- default errorの設定
- mapの対応

## sqs wrapper

```
go doc github.com/aws/aws-sdk-go
```


シンプルにインターフェイスを覗いてみてdriverという形で切り出せないか。。

```go
type Driver interface {
	ReceiveMessage()
	SendMessage()
	DeleteMessage()
}

type BatchDriver interface {
	ReceiveMessageBatch()
	SendMessageBatch()
}
```

clientというふうに考えると、こうだけれど。Receiver,Senderという形にして良いのでは？
いや、でも、driverという名前がしっくりきそうなのは全部を追加したとき。

