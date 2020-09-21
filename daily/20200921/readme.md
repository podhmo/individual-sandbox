## go interface

- 同じパッケージにinterfaceを置く
- interfaceと実装を同じパッケージに置くことで起きる問題
- 異なるパッケージに置く (2者関係, 3者関係)
- interfaceとデフォルト実装
- interfaceと実装の階層


## go db

- todo list
- user, board, lane, card

## go 考えたいこと

apikitとかwebkitみたいな名前でWAFを作ってみる（？）

- a.1 controllers, interactorsの意義
- a.2 custom handler and lifting
- a.3 app session
- a.4 client and driver pattern
- a.5 test-log (with inject logger into context)
- a.6 runtime openAPI doc generatoin (?)

もう少し考えたいこと

- b.1 fiberでの対応
- b.2 gorpを使うように変えてみる
- b.3 app sessionでfile baseのJSON
- b.4 GAE対応
- b.5 whole validation
- b.6 sqsの消費
- b.7 lambdaへのdeploy

apikit?

- errorの基底を取り出す, loggerでverbose output
- JSON logとそれ以外の切り替え
- test-log
- client library with interceptor

## go もう少し考える

本当にminimumな状態から考えてみるのが正しい気がする。

- todolist

どういうuiがあるかということを考える。

- cli cmd
- interactive shell
- web api

interactive shell的なものを何も考えずに実装してみる。

./example_architecture/00minimum

### 追記

何が困るの？

- testが書けない
- 他のuiを利用できない

どういう機能があるの？

- ui

  - interactive shell
  - parser
  - output
  - error handler
- controllers
- interactor
- router (command dispatcher)
- commands

  - list
  - add
  - done
  - help
- store
  - save()
  - load()
  - list()
  - update()

### もう少し考えてみる

controller, interactorってどういうもの？

`/xxx/{xId}` にはどう対応する？

controllerはあくまで内部の表現に揃えるもの。そんなわけでqueryでとってきたりはしない。

```go
type XIDRef struct {
  XID string
}
```

例えばこういうstructに変換するということ。

### nil 対応

```go
func SendArray(w http.ResponseWriter, r *http.Request, items interface{}) error {
	if items == nil {
		items = []bool{} // zero length array
	}
	// TODO: https://opensource.zalando.com/restful-api-guidelines
	render.JSON(w, r, map[string]interface{}{"items": items})
	return nil
```

これを許すには、untyped nilが返るようにしないとだめ。
いっその事ここで補填は辞めるかreflectを使う？(sliceの型がわからないので無理)

## go clean architecture?

覗いてみるか。

- https://github.com/Le0tk0k/go-rest-api


```console
$ tree -d
.
├── docker
│   ├── api
│   └── mysql
│       └── db
├── domain
├── infrastructure
├── interfaces
│   ├── controllers
│   └── database
└── usecase

10 directories
```

- dockerディレクトリって何者？
- domain,infrastructure,interface,usecase
- main.goはどこ？

### dockerディレクトリって何者?

- Dockerfile置き場っぽい。
- volume用の場所も兼ねている？

## domain,infrastructure,interface,usecase

mainから覗いていくと以下の様な感じ。

```
main
  infrastructure
```

めんどくさいしimportを覗くか。

```console
$ xxx() { go list -f "{{if not .Standard}}{{.Imports}}{{end}}" $1 | tr " " "\n" | sed 's/^\[//; s/\]$//'; }
$ xxx2() { xxx $1 | grep Le0tk0k; }

$ xxx2 .
github.com/Le0tk0k/go-rest-api/infrastructure

$ xxx2 ./infrastructure/
github.com/Le0tk0k/go-rest-api/interfaces/controllers
github.com/Le0tk0k/go-rest-api/interfaces/database


$ xxx2 ./interfaces/controllers
github.com/Le0tk0k/go-rest-api/domain
github.com/Le0tk0k/go-rest-api/interfaces/database
github.com/Le0tk0k/go-rest-api/usecase

$ xxx2 ./domain

$ xxx2 ./usecase
github.com/Le0tk0k/go-rest-api/domain
```

んー。本体はdomain?

```
main
  infrastructure
    interfaces/controllers
      domain #=0
      interfaces/database #=1
        domain #=0
      usecase
        domain #=0
    interfaces/database #=1
      domain #=0
```

### web側を担っているのは何？

```console
$ git grep -l echo
README.md
go.mod
go.sum
infrastructure/router.go
```

infrastructure/router.goか。んー。
portに当たるものって何？

CLIだけ作ろうとしたときには何をimportすれば良いんだろう？
webに依存したくないときに、達成できなくない？
infrastructureにそのまま全部だと全部が全部importされてしまわない？

sqlhandler.goはnamespace的な概念がなくて良いのか？

### interfaces/controllerは？

うーん。機能としてはきらいじゃないけど名前が微妙じゃない？いっその事intractorで良いのでは？
実装は嫌いではない。

```
type UserController struct {
	Interactor usecase.UserInteractor
}

func NewUserController(sqlHandler database.SqlHandler) *UserController {
	return &UserController{
		Interactor: usecase.UserInteractor{
			UserRepository: &database.UserRepository{
				SqlHandler: sqlHandler,
			},
		},
	}
}

func (controller *UserController) CreateUser(c Context) (err error) {
	u := domain.User{}
	c.Bind(&u)
	user, err := controller.Interactor.Add(u)

	if err != nil {
		c.JSON(500, NewError(err))
		return
	}
	c.JSON(201, user)
	return
}
```

primitiveなobjectはdomainでそれは理に叶っていそう。

### usecaseとは？

usecaseの中にinteractorとrepositoryを入れちゃうのか。
build時の依存のことを考えることってないのかな？
まぁでもinteractorとrepositoryがここにあってもありといえばありか。
interactorに渡されるrepositoryはinterfaceなのか。。storeとかそういう名前のほうが好き。

interactorが保持するinterfaceの実装自体は、interfaces/databaseのところ。
うーん、interfacesではなくimplementsくらいのほうが良いのでは？ (implementation?)
（まぁclean architectureがinterfacesだから）

### main.goはどこ？

./server.go っぽい。何かdocker-compose経由で起動する模様。

やっていることはこれだけ

```go
infrastructure.Init()
```

環境変数やconfig fileのhandlingはどこでやるんだろう？
