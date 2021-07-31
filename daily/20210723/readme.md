## ミニマムなものを考える

以下を同時にやれば？

- docgen
- webgen

```go
Mount("/users", GetUser).After(func(op *openapi3.Operation){
    doSomething(op)
})
```

ただ、可能なら実装の詳細を保持したくないよなー。
あと、Input,Outputも結局生成することになるのでは？ pathとかどうする？
結局、interactorのパラメーターはInputを持っているので良いような気がしている。

handlerの作成とmountを同時にやっちゃいたい。
ここで、コード生成とバッティングしてしまうみたいな話しか。。

### 追記

new typeの実装はきれいに書けそうなのだけれど、依存を注入する方法がほぼcontextのみになる。
色々考えたけれどwebglueみたいなパッケージ作ってしまうのが楽なのでは？

```
func Mount(router webglue.Router) {
    router.AddEndpoint("/foo", "POST", Foo, NewFooHandler(Foo, resolver))
}
```

## go 最高のapp structure

全体

- pkg -- これは合っても良いし無くても良い
- glue -- appとpkgをつなぐもの 例えばlogglue, configglue, dbglue
- app -- これは作るか決まっていない

domain/region的な何か

- foo/design -- これだけ見ればわかるようになっていると最高 (無いかも)
- foo/footest

  - ここでfixtureのsetup,teardownを作りまくる
  - 実は、CreateLoginUser()のような関数などを作っても良いのでは？

- foo/web

  - ここで実装をmountする

### fastAPI likeな実装

webのhandlerは自動生成したい。
DI的なものをどうするか？という話になる。
これはinterfaceを用意してあげれば良いのでは？

componentsを末端的なものに揃えると良いという説

- 例えば、`*databse.Session` のようなものではなく `*sqlx.Db` だけ
- 戻り値としてerrorを取りうる可能性を抑制する `*database.Session` などについては

もう少し、DIをアグレッシブに使えないものか

- 認証的なものをこちらで対応する `GetLoginUser()` ではなく `AuthenticatedUser()` 
- 型を分けてそれを要求するというような定義にしてみる (AuthenticatedUserとUserは別物)

最終的に束ねて大きな定義にしたい

- rootでembed

### handlerの実装

```go
// http handler (生成したい)
func Handler(w *http.ResponseWriter, r *http.Request) { ...}

// interactor (action, usecase)
func Interactor(
    ctx context.Context,
    dbSession *db.Session,, // need interface { DBSession(ctx context.Context) (*db.Session, error) }
    authenticatedUser *auth.AuthenticatedUser, // login required
) (Output, error) {
    // dbSession自体はAPIErrorを返す？
}
```

関数がメソッドを持てることの意味って何だろう？

```go
type Foo action.Foo // これってそのまま呼べるのだっけ？Fooが関数の場合
func (f *Foo) ServeHTTP(w *http.ResponseWriter, r *http.Request) {
    output, err := func(ctx context.Context) (Output, error) {
        ctx := r.Context()
        resolver := NewResolver(r)

        dbSession, err := resolver.DBSession(ctx)
        if err != nil {
            return nil, err
        }
        authenticatedUser, err := resolver.AuthenticatedUser(ctx) // dbSessionが必要になる場合もある？(戻り値でlookup,不足しているなら名前でも)
        if err != nil {
            return nil, err
        }
        return f(ctx, dbSession, authenticatedUser)
    }()
    if err != nil {
        handleError(err) // この辺はliftする感じで良い気もする
        return 
    }
    handleSuccess(output)
}
```

## go 関数をinterfaceに

- net/http.HandlerFuncのようなもの
- 実装するには型を定義する必要があった

```
design.FooAction
interactor.Foo
web.Foo
```

こうなってくると、何個も書くのがバカバカしくはないか？
これが、designとか用意すると面倒になる点の一つだよなー。

ちなみにreflectで取ろうとすると、見つかるのはinteractor.Fooのもの