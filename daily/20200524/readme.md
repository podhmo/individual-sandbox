## gglgen

- https://gqlgen.com/getting-started/
- https://dev.to/glyphack/introduction-to-graphql-server-with-golang-1npk

こうすると概ね試せる。

```console
$ go run github.com/99designs/gqlgen init
```

## restAPI

go-chi経由でいろいろするのだるい。本当に必要？

## go grpc

まぁ慣れれば書けなくもない。

[index.html](example_go/05grpc/index.html)を生成できるのが便利。

## go zenrpc

- github.com/semrush/zenrpc/zenrpc
- GET ":9999?smd" とかするとschemaが見れる
- `//go:generate go run github.com/semrush/zenrpc/zenrpc` とか書いておくと便利

```console
Generated services for package internal:
- ArithService
  • Sum(a int, b int) (int, *zenrpc.Error)
  • Multiply(a int, b int) int
  • Divide(a int, b int) (quo *Quotient, err error)
  • Pow(base float64, exp float64) float64
```

```console
$ echo '{"jsonrpc": "2.0", "method": "multiply", "params": {"a": 42, "b": 23}, "id": 1}' | http --json POST :9999/
HTTP/1.1 200 OK
Content-Length: 37
Content-Type: application/json
Date: Mon, 25 May 2020 01:09:28 GMT

{
    "id": 1,
    "jsonrpc": "2.0",
    "result": 966
}
```
## go openAPI上の実装で必要なものについて考えてみる

## go graphql with protobuf

- https://github.com/ysugimoto/grpc-graphql-gateway
- https://github.com/google/rejoiner
- https://github.com/grpc-custom/graphql-gateway

特に気にしているのはgrpc-graphql-gateay。grpcに対してgatewayを提供した
ということはわかるんだけど。何をbatch requestしているのだろう？

結局、素直にprotobufでgraphqlの定義を書いているだけ？

## go output側の挙動を考えてみる

- ふつうにfieldに乗せてみる
- 内部のmodelだけをフィールドとして取るものを考えてみる

そもそも必要な理由を考えてみると、形状が異なるから。例えばパスワードな
どの値はそのまま出力に含まれてほしくない(もっとも、パスワードなどをユー
ザー情報のそれと同じ位置に保存するのは論外という気もするけれど。そこは
それふつうは存在しないageというフィールドがよく例の中には存在している
のと同様の話)。

### 追記

new typeで指定してみるのはどうだろう？

### 追記

embeddedで作ってみるのはどうだろう?

考えてみるとこれは複数のmodelをまとめて保持したい時に不便。
