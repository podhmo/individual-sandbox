## 今日

してみたい？

- lambda + api gateway + (openAPI doc)
- githubのissueを横断して検索するくん
- client,driver pattern
- go-chiのdocgenを覗いてapi docの生成について考えてみる
- permission with app session

  - without db
  - with db
- 歯医者
- ent/gorpの利用？
- sql query log

## go sql query log

https://github.com/simukti/sqldb-logger

entに対しても適用は手軽。ただgo.modが悲しい感じ。

## go go-chi permission

With()の使いみちがわかったかもしれない。
paginationの他に。
djangoなどで言うlogin_required()のdecoratorに通ずる物があるのでは？
これをdocに連携させたい。


### doc

- https://github.com/go-chi/docgen

これがどこまで表示できるかだよなー。昔に使ってみていた。
[../20200501/example_go/09chi-rest/README.md](../20200501/example_go/09chi-rest/README.md)

うーん

- middlewareのnestが見える
- pathに対応した関数が分かる雰囲気がある
- routesの数がわかる

これだけか。これは微妙だ。やっぱり戻り値の関数とかがわからないから仕方がないね。。

## go

- https://github.com/opencensus-integrations/ocsql

わりと好きなやつでは？


## go runtime reflection

意外と悩ましいな。

- interactorの情報を上手くrouterに渡せない
- interactorをそのまま受け取るようにしてしまうと、全てのパッケージへの依存になる
- interactorをinterfaceにしてしまうと別途実装する必要が出てくる

あと、interactorに依存があった場合にはinteractor自身を渡せないなー。

- go-swaggerのcontroller生成と何が違うんだろうか？
- mountを行うのではなくcontrollerは生成してしまう
- 結局の所、params固定responseは？errorを返す

もしかして、errorが返せるgo-swaggerでは？
違うな。自分で定義したstructが使えるのが利点だ。inputがyamlではないから。

### 追記

どんどんgo-swaggerに近づいていってしまう？
型を自分で指定するのは嫌だな。

entryは本物を使えば良いだけ？
頑張って指定して行けば良いのでは？

gqlgenにも近づいていきそう。

### 追記

いろいろ考えたけど、docの生成に時間がかかるのは許してもらうと言う形が無難かも。
そんなわけでinterfaceを切らない。interactorの。

### 追記

app sessionはどうしよう？

```
List()
List(context.Context)
List(context.Context, in)
```

interfaceとしてみれば良いのでは？そしてinteractorが持てば良い。
ただし、持つのはinterfaceのfactory。

これはこれでめんどくさそう。

### api doc?

とりあえず、api.txtみたいなものを吐けば十分では？

```
GET /api/todo TodoInteractor.List func(ctx context.Context) ([]Todo, error)
GET /api/todo/{todoID} TodoInteractor.Get func(ctx context.Context, todoID string) (Todo, error)
```

permissionを設定したい。

## apikit

- handling error
- not implemented error
