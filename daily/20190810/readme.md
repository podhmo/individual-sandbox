## flutter widget

- https://flutter.dev/docs/development/ui/widgets
- https://qiita.com/matsukatsu/items/e289e30231fffb1e4502
- https://qiita.com/coka__01/items/dedb569f6357f1b503fd

### performance considerations

- https://medium.com/flutter-jp/dive-into-flutter-4add38741d07
- https://api.flutter.dev/flutter/widgets/StatefulWidget-class.html#performance-considerations


## go gqlgen

- https://gqlgen.com/reference/dataloaders/
- https://github.com/99designs/gqlgen/tree/master/example/dataloader

## eglot 全てのserverをshutdownさせたい

- shutdownはeglot-shutdown

```
(cl-loop for servers
  being hash-values of eglot--servers-by-project
  do (cl-loop for s in servers do (eglot-shutdown s)))
```

## go sqlxの素振り的なことをしないと？

- https://jmoiron.github.io/sqlx/
- sqlite3のinmemory dbを利用する？
- query logだす？
- context対応したコードはどういうやつ？

使いかたはここを見れば良さそう

- https://github.com/jmoiron/sqlx/blob/master/sqlx_test.go

misc

- https://qiita.com/k-motoyan/items/f37d1348efd3f40e9096
- https://snippets.aktagon.com/snippets/757-how-to-join-two-tables-with-jmoiron-sqlx
- https://qiita.com/rihofujino/items/b69e6a23e7cef1d692c4

### sqlite3 trace log

pythonのはこれだけど何かあるはず。

- https://stackoverflow.com/questions/6941992/how-can-i-log-queries-in-sqlite3-with-python

どうやらタグ付きでbuildすれば良いらしい？

- https://github.com/mattn/go-sqlite3/wiki/Features#usage

そういえばgo getでタグ付きでbuildするのどうやるんだろ？

 - https://stackoverflow.com/questions/30188499/how-to-do-go-get-on-a-specific-tag-of-a-github-repository

無理？結構めんどくさそう？

### sqlx tips

- ctxを使っている方の関数を使いたい
- QueryRowxを使うと便利

```go
var p Place
err := db.QueryRowx("SELECT city, telcode FROM place LIMIT 1").StructScan(&p)
```

## coveralls?

なにもの？

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

- stableは@latest
- nightlyは@master

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
