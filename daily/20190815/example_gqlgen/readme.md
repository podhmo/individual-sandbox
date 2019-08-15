# gqlgenとかのexampleを見る

- https://github.com/99designs/gqlgen/tree/master/example/dataloader

## todo

まじめに読むか。

### todo/server

- handler.Playground()
- handler.GraphQL()

こういうhandlerが用意されていて（正確に言うとhandler mapper的な感じ）
ほとんどnet/httpの部分のサーバーと同じような感じ。

- graphQLのhandlerはschemaを必要とする
- context.Context, interface{} -> error というhandlerを受け取る
- いや上のやつはただのrecover handlerかも。

何が分かっていないんだろう？何が生成されて何が自分で実装するかが分かっていない感じ？
まぁ明らかなのはschemaから関連するような型は生成されるし。resolver部分は実装しないといけない感じではありそう。

### todo

todo.goが本体っぽい気がする。 `todo.New()` が Configを生成する。
こいつがinterfaceを実装したものなんだろうか？

あー、読み間違えていた。
Schema factoryがconfigを受け取り、Schemaを生成する。このSchemaはinterfaceを持つ感じ。

```go
type ExecutableSchema interface {
	Schema() *ast.Schema

	Complexity(typeName, fieldName string, childComplexity int, args map[string]interface{}) (int, bool)
	Query(ctx context.Context, op *ast.OperationDefinition) *Response
	Mutation(ctx context.Context, op *ast.OperationDefinition) *Response
	Subscription(ctx context.Context, op *ast.OperationDefinition) func() *Response
}
```

あ、そもそもがこれに依存しているのか。

- https://github.com/vektah/gqlparser

もともと生成に必要とされるファイルは.ymlで

```yaml
models:
  Todo:
    model: github.com/99designs/gqlgen/example/todo.Todo
  ID:
    model: # override the default id marshaller to use ints
      - github.com/99designs/gqlgen/graphql.IntID
      - github.com/99designs/gqlgen/graphql.ID
```

みたいな設定を渡す。modelsのIDがなにかがよく分かっていないかも？
なんで配列を渡せているのがわからんない。あー、union typeを模したような感じなのか。
ただここの部分は直接的な型があるわけじゃないっぽい？MarashalID,UnmarshalIDみたいな感じの関数が定義されている感じ。

こういうような型を渡しているっぽい。特別なタグを定義したりしているわけじゃなさそう。

```go
type Todo struct {
	ID    int
	Text  string
	Done  bool
	owner *User
}
```

schema.graphqlは用意されていそう。

### test的なもの

- gqlgenのClientを利用して実行するっぽい。

全然関係ないけれど。 `MustPost()` というようなメソッドをもたせるのは楽なのかもしれない？
clientは結構だいぶラップされていそう。`Var()` ってなんだ。mapに追加するfunctional optionか。

あー、mapstructureってmap <-> struct{}の変換なのか。

## そもそも構造がはあくしきれていない気がする

```
.
├── generated.go
├── gqlgen.yml
├── models.go
├── models_gen.go
├── readme.md
├── schema.graphql
├── server
│   └── server.go
├── todo.go
└── todo_test.go

1 directory, 9 files
```

この内inputになるのはgqlgen.ymlとschema.graphql。server/server.goはほとんどコピペ的な感じ。
generated.goとmodels_gen.goはたぶん自動生成。enerated.goはほとんどruntimeみたいなもの。

残ったファイルは以下

- models.go
- todo.go

この内todo.goはどうもscaffoldによって初期状態が生成されていそう。todo.goというなまえはわかりづらいけれど。実質resolverとそれを作成するためのfactory(+ config)。

そしてmodels.goはほとんどただのstructとinterfaceの定義。なるほど。

## dataloader

ようやく本題。こちらも最初に構造を載せていく。

```
.
├── addressloader_gen.go
├── dataloader_test.go
├── dataloaders.go
├── generated.go
├── itemsliceloader_gen.go
├── models_gen.go
├── ordersliceloader_gen.go
├── readme.md
├── resolvers.go
├── schema.graphql
└── server
    └── server.go
```

これらに対しても同様に不要そうな情報を除外していこう。

以下は生成されたコードのはず

- addressloader_gen.go
- generated.go
- itemsliceloader_gen.go
- models_gen.go
- ordersliceloader_gen.go

以下は実装に貢献していないコード

- dataloader_test.go
- readme.md
- schema.graphql

残ったものは

- dataloaders.go
- resolvers.go

とはいえserver/server.goから見ていくのがいちばん手軽だと思う。

### server/server.go

go-chi/chiをrouterとして使っているくらい？
そしてdataloaders.goあたりで定義していたDataLoaderがhandlerのmiddlewareとしてつかわれている。まぁ確かにmiddlwareのところでやるのが自然な気がする。





