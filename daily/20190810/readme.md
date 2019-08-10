## そういえば

- graphqlのformatterがないな
- linterも

## webの何か(snapshot testを試すための)

この辺の裏側を置き換えるとかどうだろう？

- https://github.com/tiangolo/full-stack-fastapi-postgresql

せっかくだしGraphQLを？

- https://github.com/99designs/gqlgen/tree/master/example

## csv SQL

これに似たような名前の何かがあった記憶

- https://github.com/noborus/trdsql


## go defer そういえばdeferにメソッドを渡すやつあったな

これ考えてみれば何もむずかしいことなくない？

## go go-chi

[../20190806/README.md](../20190806/README.md)

### 追記

そういえば Router, Group, の概念をごっちゃにしているかも。

- RouterはMount()できる
- RouterはGet(),POST(),...などを持っている
- Group()は

## go modules

### sub packages

そういえばsub packageでだけ依存が欲しい場合のgo.modどうなるんだろ？

### ambiguous import

なんかこういうエラーがでるな

```
go: extracting github.com/podhmo/ctxlog v0.0.0-20190809192820-b371af1f1a78
can't load package: package github.com/podhmo/ctxlog/zapctxlog: unknown import path "github.com/podhmo/ctxlog/zapctxlog": ambiguous import: found github.com/podhmo/ctxlog/zapctxlog in multiple modules:
        github.com/podhmo/ctxlog/zapctxlog ($GOPATH/src/github.com/podhmo/ctxlog/zapctxlog)
        github.com/podhmo/ctxlog v0.0.0-20190809192820-b371af1f1a78 ($GOPATH/pkg/mod/github.com/podhmo/ctxlog@v0.0.0-20190809192820-b371af1f1a78/zapctxlog)
```

- https://github.com/golang/go/issues/26904

しょうがないのでコードを読もう。

https://github.com/golang/go/blob/9c1f14f3763dbbc0d8105fa88c0b7bdad5674ee7/src/cmd/go/internal/modload/import.go#L132

ここで発生しているもの。まぁエラーメッセージの通りにmoduleの候補が複数ある。

### 追記

いろいろかんがえたけれど。モジュールとパッケージは異なりモジュール単位では相互に参照しあっているのも問題。

## go logger zap

そういえばzapの使いかたを把握していないな。

- https://github.com/uber-go/zap

## go ctxlogger

- contextual logger
- structural logger
