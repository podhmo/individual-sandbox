## 今日

してみたい？

- lambda + api gateway + (openAPI doc)
- githubのissueを横断して検索するくん
- ok client,driver pattern
- ok go-chiのdocgenを覗いてapi docの生成について考えてみる
- permission with app session

  - without db
  - with db
- 歯医者
- ok ent/gorpの利用？
- ok sql query log
- apikitに欲しい物の整理
- ok hanamiを読む 

## reqtest

- 名前が良くない reqtest (requestと打ち間違える）
- webtestと違うの？

  - recorderに対応していない
  - snapshotが地味に便利

- application JSONなどに変更する
- cookie,header,form data, json

## 試す

- clientを複数

  - driverという括りにすると綺麗？
  - 本体がライブラリに依存したくない
  - 名前がまだ不思議な感じ
  - たぶん通知系のコードがslackという名前をつけているのが良くない

- `?pretty=1`対応 (renderer)
- parserを作ってみる (parser?)

  - 変に関数を分ける必要はないのでは？ -> ない理由はrequiredなどのチェックがないため
  - エラーの対応などのほうがだるい
- authを考える

## go apikit

- log
  - request/response log
  - json log / text log
  - test log
- app session
  - changes db -> transaction, easily (app scope -> request scope)
  - dependencies handling, easily
  - caching request scope component
- custom handler
  - application error
  - unwrapped error (response) and verbose error (log)
- apitest
- (permission)
  - authentication
  - authorization
  - login/logout/refresh
  - test helper for permission
  - session?
  - api key
- (pagination)
- (format check)
  - parse JSON
  - parse query
  - parse path
  - data binding
  - error message
- (interactor)
- (api doc)
- (code generation)

## format check

長い型を書きたくない。
interactorに個別の引数を並べたくない(?)
とはいえ、いっその事fastAPIのように、、と思ったがgenericsがないので厳しそう。

- entなどのようにfieldを個別に値として定義するか
- structのfieldとtag

### 思ったこと

すでに存在する値にくっつけるならbind。そうでないならparse。
値を得るのにrequestが必須という状況は避けたい？
(parametersをどうする？, bodyをどうする？)

こんなやり方もありか。。？前者で長い型名を書きたくない。

```
func List(context.Context, ListInput) ([]Item, error)
func List(context.Context, ...ListOption) ([]Item, error)
```

bindが嬉しいのはどのような型の値が使われるか分かるからなのだけど。。

```
var ob ListInput
c.Bind(req, &ob) // なぜこれではだめなのか？mapが返ってくるみたいなものはコレで行ける。

parser.ParseListInput(reader) // methodが増えていく

parser.Todo().List().ParseWhole(reader) // path含む
parser.Todo().List().Parse(reader) // bodyのみ
```

## authentication

login userはやっぱりcontextに持っていたほうが楽なのでは？
とはいえ、入るときのことを考えるとrequestは必要なのか。
でも、interactorはrequestに依存したくない。-> interfaceへの依存に。
たぶん app sessionがrequest,responseを持つというのが正しそう。


## client, driver pattern

slack clientについてはできた。他の何かを気にして見る必要はある？

- api client
- job queue

api clientというか複数のcontextを持ったような何かか。awsとかgoogleのapiみたいなネストしそうな何か。
あとは、実行結果の戻り値を使いたい場合(not notificator)。

あとは、call countを気にしたいみたいな話もあるのか。それでfactoryをつくるみたいな
これは別の話になるのでは？

### api client

- request assertion
- response modification
- register mock response
- (可能なら、本物と通信して、自動で更新）

## go sql query log

https://github.com/simukti/sqldb-logger

entに対しても適用は手軽。ただgo.modが悲しい感じ。

## go go-chi permission

With()の使いみちがわかったかもしれない。
paginationの他に。
djangoなどで言うlogin_required()のdecoratorに通ずる物があるのでは？
これをdocに連携させたい。

### params

go-chiのContextを利用してよいか？

- URLParams
- RouteParams

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
