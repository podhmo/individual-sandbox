# apikit

理想というわけではないけれど、こういうWAFがあったら便利なんじゃないか？というようなアイデアをメモしておく。
基本的な方針は、関数定義をもとにDIをする形。DI自体はコード生成によって行われる。
(fastAPIの使用感から着想を得た感じのもの。1ファイルにすることは難しそう)

以下の様な形で生成する対象を全て自分で書き下す。

design/recipe.go

```go
package design

func ListUser(db *DB) ([]*User, error) {
    return db.Users(), nil
}

type GetUserInput struct {
    UserID string `path:"userID"`
}
func GetUser(db *DB, input GetUserInput) (*User, error) {
    return db.GetUser(input.UserID)
}

func NewRouter() *Router{
    r := NewRouter()
    // ここでmetadataを挿入できるのが便利    
    r.AddAction("/users", "GET", ListUser)
    r.AddAction("/users/{userID}", "GET", GetUser) // 素直に引数にstructを定義する？もしくは引数名と同名のパスを見る？ (userID),
    // r.AddRouter("/xxx", subrouter)
    return r
}

func main(){
    pkg := NewPackage("./app", "app")
    cpkg := NewPackage("./compnent", "component")

    emitter := NewEmitter()
    emitter.Start()
    defer emitter.Emit() // goroutineなどを使って高速に走らせたほうが良い？

    rgen := NewRouterGenerator(emitter, pkg)
    hgen := NewHandlerGenerator(emitter, pkg)
    cgen := NewComponentGenerator(emitter, cpkg, WithLiftHandler(component.LiftHandler))

    r := NewRouter()
    for _, ac := range r.Actions {
        h := hgen.Generate(fmt.Sprintf("%s.go", ap.Name(), fmt.Sprintf("New%sHandler", ap.Name)))
        rgen.AddHandler(h)
        cgen.Capture(ac)
    }

    rgen.Generate("router.go", "NewRouter")
    // rgen.GenerateDoc("openapi.json") // 出力できても良い

    cgen.Provide("DB", func() (*DB, error) { }) // 実際の実装を登録する？
    // cgen.Provide("OtherDB", func(otherDBURI string) (otherDB *DB, err error) { })
    // actionの引数名と戻り値が一致するようなものを登録できるようにしても良い？
    // 依存を引数として登録してあげると、config的なものを生成できるようにしても良いかもしれない

    cgen.Generate("provider.go", "Provider", cgen.Interface())  
    // cgen.Generate("config.go", "Config", cgen.Config())  // 引数として使われたConfigを生成するようにしても
}
```

## 生成されるファイル達

以下のファイルなどは自動生成される。ただし、この辺もフレームワークとして提供すると言うよりはそのような生成を記述するためのツールキットが存在くらいのほうが嬉しい気がしている。

component/provider.go

```go
package component
type Provider interface {
    DB() (*DB, error)
    // 戻り値が *DB, (*DB, func() error), (*DB, error) などの可能性がある？。指定しない場合は *DB
}
```

app/router.go

```go
func NewRouter(p component.Provider) http.Handler {
    r := chi.NewRouter()
    chi.Get("/users", NewListUserHandler(p))
    chi.Get("/users/{userID}", NewGetUserHandler(p))
}
```

app/ListUser.go

```go
// このinterfaceはバラバラになったもの使うかもしれない

func NewListUser(p component.Provider) http.Handler {
    return component.LiftHandler(func(w http.ResponseWriter, req *http.Request) (interface {}, error) {
        db, err := p.DB() // session等の場合の可能性もある。そのときにはhandlingが必要。
        if err != nil {
            return nil, err
        }
        // main.goに実装があると困るかも。その場合はコピーする？微妙。まぁaction以下にあるとする。
        return action.ListUser(db)
    }
}
```

app/GetUser.go

```go
// このprovider interfaceはバラバラになったもの使うかもしれない
func NewGetUser(p component.Provider) http.Handler {
    return component.LiftHandler(func(w http.ResponseWriter, req *http.Request) (interface {}, error){
        var input action.GetUserInput
        input.UserID = chi.URLParam(req, "userID")
        // validation 的な操作も入る？

        db, err := p.DB()
        if err != nil {
            return nil, err
        }
        return action.GetUser(db, input)
    }
}
```

## 自分で記述するファイル達

生成はされないが一番外側のlift関数もあるとうれしい。この辺runtime?

```go
type Handler func(http.ResponseWriteer, *http.Request) (interface {}, error)
func LiftHandler(h Handler) http.HandlerFunc {
    return func(w http.ResponseWriter, req *http.Request) {
        cap := captureStart(req)
        defer cap.End() // for tracability

        r, err := h(w, req)
        if err != nil {
            doError(w, req, err)
            return
        }

        // 配列のときには特定の形状に変換して返したいかもしれない (e.g. {items: [], more: true})
        doSuccess(w, req, r)     
    }
}
```

自分でProviderは実装しないとダメ

main.go

```go
type Config struct {
    DBURI string
}

type Provider struct {
    Config Config
    DBFactory *DBFactory
}
func (p *Provider) DB() (*DB, error) {
    return p.DBFactory.Session(p.Config.DBURI)
}

func main(){
    p := &Provider{Config: Config{"DBURI": "sqlite://:memory:"}}
    r := app.NewRouter(p)
    if err := http.ListenAndServe(r, ":8080"); err != nil {
        log.Fatalf("!! %+v", err)
    }
}
```

このときの階層はどうなっているんだろう？

```
- go.mod
- design/recipe.go (main.goという名前のほうが良いかも？)
- action/actions.go
- component/lift_handler.go
- component/provider.go -- generated
- app/router.go -- generated
- app/ListUser.go -- generated
- app/GetUser.go -- generated
- cmd/webserver/main.go
```

goaに近いがgoaほどgeneratorとuser codeが分離されていない状態。