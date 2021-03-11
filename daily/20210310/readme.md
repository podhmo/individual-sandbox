## runtime/debug

覗いていた。そういえばpaniclogどうしてたっけ？allは不要そう。
panicerr


## go sql lint

- https://github.com/stripe/safesql
- https://github.com/houqp/sqlvet

### go callgraph

- x/tools/go/callgraph
- https://pkg.go.dev/golang.org/x/tools/go/pointer
  - https://github.com/golang/go/issues/39604 
- https://pkg.go.dev/golang.org/x/tools/go/ssa
- https://pkg.go.dev/golang.org/x/tools/go/ssa/ssautil

### ssa

- https://ajalab.github.io/2018/05/02/go-ssa.html

ssaにするとどれに何が入るかがわかって便利と言う話でlintの時には使える。

## go 一からweb apiを作るとしたら

- webhandlerみたいな形で取り出す

  - interactorと分割するがここでinterfaceを挟むかは悩みどころ
  - 関数の型を渡すとか出来たっけ > reflectopenapi

- dbはマルチテナント前提にしてdrop database, create databaseは1回だけ
- 実装の候補は何が良いだろう？

  - RBAC的なpermission管理
    - dynamic filter的なものはどうしようか？ (条件が返ってくれば良いのでは？)
  - realdowld example

- web apiを実装したら欲しいもの

  - openapi doc
  - どういうresourceがあるかのendpoint
  - client


## go tenuki

- graphQLの実行例も作ってみる
- jsendはexamplesに入れてあげたら？
- connectionが切断される系のエラー

## goで通信を妨害したい

- Transportでエラーを作ってあげれば良い？
- hijackerあたりでアレコレできない？
  - https://gist.github.com/Soulou/6048212
