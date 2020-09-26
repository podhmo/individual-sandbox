## go interactorを定義してcontrollerを生成が正しいのでは？

ついでにlambda+api gatewayのことを考える

### 実はlambdaの形式が一番正しいのではないか？

- https://docs.aws.amazon.com/lambda/latest/dg/golang-handler.html

何が問題なんだろう？関数のsignatureが個々のhandlerによって変わる点？
許されるのはこれら。

```
func Start(handler interface{})
    Start takes a handler and talks to an internal Lambda endpoint to pass
    requests to the handler. If the handler does not match one of the supported
    types an appropriate error message will be returned to the caller. Start
    blocks, and does not return after being called.

    Rules:

        * handler must be a function
        * handler may take between 0 and two arguments.
        * if there are two arguments, the first argument must satisfy the "context.Context" interface.
        * handler may return between 0 and two arguments.
        * if there are two return values, the second argument must be an error.
        * if there is one return value it must be an error.

    Valid function signatures:

        func ()
        func () error
        func (TIn) error
        func () (TOut, error)
        func (TIn) (TOut, error)
        func (context.Context) error
        func (context.Context, TIn) error
        func (context.Context) (TOut, error)
        func (context.Context, TIn) (TOut, error)

    Where "TIn" and "TOut" are types compatible with the "encoding/json"
    standard library. See https://golang.org/pkg/encoding/json/#Unmarshal for
    how deserialization behaves
```


あと、`lambda.Start()`はHandlerを受け取るわけじゃないのかな？Handler自体はこういうinterfaceっぽい。

```
type Handler interface {
	Invoke(ctx context.Context, payload []byte) ([]byte, error)
}

func NewHandler(handlerFunc interface{}) Handler
    NewHandler creates a base lambda handler from the given handler function.
    The returned Handler performs JSON deserialization and deserialization, and
    delegates to the input handler function. The handler function parameter must
    satisfy the rules documented by Start. If handlerFunc is not a valid
    handler, the returned Handler simply reports the validation error.
```


まぁpayloadで`[]byte`を見てこれをJSONにencode/decodeして使うという形。

