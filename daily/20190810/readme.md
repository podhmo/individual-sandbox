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

## go logger zap

そういえばzapの使いかたを把握していないな。

- https://github.com/uber-go/zap

## go ctxlogger

- contextual logger
- structural logger
