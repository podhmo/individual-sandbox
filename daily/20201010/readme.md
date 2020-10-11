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

### default errorの設定

せっかくだし一般的なvalidatorを探してみるか。ginあたりが組み込みだった記憶。
たぶん有名なのはこの辺り。

https://github.com/go-playground/validator


## go errors.As()

対象のerrorが値のときにうまくいかない？

```go
func As(err error, target interface{}) bool {
	if target == nil {
		panic("errors: target cannot be nil")
	}
	val := reflectlite.ValueOf(target)
	typ := val.Type()
	if typ.Kind() != reflectlite.Ptr || val.IsNil() {
		panic("errors: target must be a non-nil pointer")
	}
	if e := typ.Elem(); e.Kind() != reflectlite.Interface && !e.Implements(errorType) {
		panic("errors: *target must be interface or implement error")
	}
	targetType := typ.Elem()
	for err != nil {
    	// ここで、reflectlite.TypeOf(err)が ValidationErrors
        // targetTypeが *ValidationErrors になってしまう。
		if reflectlite.TypeOf(err).AssignableTo(targetType) {
			val.Elem().Set(reflectlite.ValueOf(err))
			return true
		}
		if x, ok := err.(interface{ As(interface{}) bool }); ok && x.As(target) {
			return true
		}
		err = Unwrap(err)
	}
	return false
}
```

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

