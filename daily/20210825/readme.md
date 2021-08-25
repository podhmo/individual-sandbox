## astmplの理想

引数を呼ぶように実行したい？気がしていた。
go/typesを参考にできないかと思ったけれど。頑張るのが辛そう。
自作しちゃったほうが早いかもしれない。

- factory (template) から instanceが作成される
- factoryが持つのは仮引数
- instanceに持たせたいのは実引数
- instance自身が持つのではなくinstanceを呼び出すタイミングで実引数を渡したい
- それとは別に情報を束縛できると便利？

  - パッケージの情報は保持してほしい
  - 自分自身の名前は保持してほしい
  - 引数としてやってくるのはSymbol (name, Pkg)

### 追記

もう少し整理してみる。

design/ListUser.go

```go
func ListUser() []*User {
    return users
}
```

こんなdesignがあったとして、これをrpcで利用したい。
公開するためのmarkerを付けるか自分で書く必要がある。
今回は自分で書く場合

```go
router := NewRouter()
router.Add("GET", "/users", ListUser)
```

これで以下のように動いてほしい

- http.HandlerFuncへの変換
- webAPIのrouterに登録

変換はtemplateで提供されていて、このためにtemplateの変換結果を利用したい。

```go
hgen := NewHandlerGenerater()
rgen := NewRouterGenerator()

handler := hgen.Generate(ListUser, "ListUser.go")
rgen.Register("GET", "/users", handler)

rgen.Generate("router.go)
```

↑のrouterの定義からこのgeneratorのコードをループで記述したい。

```go
hgen := NewHandlerGenerater()
rgen := NewRouterGenerator()

for _, ep := range router.Endpoins {
    // endpointはName,Method,Pathなどを持っている?
    handlerCode := hgen.Generate(fmt.Sprintf("%s.go", ep.Name), ep)
    rgen.Register(ep.Method, ep.Path, handlerCode)
}
routerCode := rgen.Generate("router.go")

component := rgen.ExtractComponent()
cgen := NewComponentGenerator()
cgen.Generate("components/interface.go", component.Interface())
```

そして、これがtext templateなどでの実装であってはいけない理由は何なのだろうか？既存のツールではダメな理由は何なのだろうか？

- DIは実行時に必要なインターフェイスを持ったものから取り出す形でやりたい
- 引数としてDIの必要とされるものを定義する
- 任意の条件をwrapしたい
- 肝はweb APIではなく、任意のactionへのglue部分の生成
- (サブパッケージとしてcomponentが切り出せると綺麗)

こう考えると、

複数のアクションをルーターに登録したい。
ルーターの実行用の関数を生成したい。
この時の依存を入力として外に出したい

みたいな感じなのかな。

### 追記

本質的に欲しいのは、web api code generatorじゃないのだよなー。
code generationベースのdependency injector。
その上で、inject結果の展開方法を自由に扱いたい。

- action -> web handler -- 関数単位でinjectされた実装を生成
- router -> web router -- ここでdependencyを取得

ついでなのでweb handlerの登録もしてしまいたい。
引数どっちがうれしいか？みたいな話は出てきそう。

### DIの他にやりたいことは何？ (web handler)

- requestのパース (*http.Requestから所定のデータの復元)

  - path, querystring, bodyを見る
  - bodyのvalidation
- outputをJSONに変換

これをactionの中で書くとどう問題になるのかといえば、Requestにアクセスする必要がでてくるせい。

必要になったら取り出す

```go
func Foo(req *http.Request) {}
// 必要ならinjectする。しかし本当に必要なのはRequest?
```
