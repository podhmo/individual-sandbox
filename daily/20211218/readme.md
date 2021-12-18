## go always valid model

always valid modelという感じの体の話をgoでの事柄について考えて笑みたい
https://scrapbox.io/kawasima/Always-Valid_Domain_Model

いろいろな論点はある

- javaベースで書かれているものをgoに持ってくるときにどうするか？
- 個人的に思っている、usecaseではなくactionとして捉えれば十分なのではないか？との対応
- そもそも、goでめんどくさくならずにいい感じにやれるか？
- 可能なら、composableな感じで定義できないか？

### always valid model

- 完全コンストラクター (setterの呼び出しをなくす)という話の進化版
- isValid()を呼ばないといけないというのが微妙
- いや、手で呼ぶのは全然構わないのだけれど、呼び忘れないようにしたい。

### actionの中身の実装

- commandとqueryの話は一旦おいておいて、commandだけ考えるとして
- commandが属性としてEmailAddress (入力値)を持つとかを持つ実装はそういえば見ない
- 通常はdbオブジェクトとかそういうやつになりがち？
- commandとしてwrappingができるといい感じに配線できる？

こうではなく。という話。

```go
type RegisterCommand struct {
    DB *DB
}

func (c *RegisterCommand) Register(input struct { EmailAddress string }) error {
    ...
}
```

### 悩ましい話

- DI的なものの対象だけがドキュメントに載るという場合に情報が隠れてしまう
- validationのための情報を書いておきたいところではあるのだが
- (例えば、openapi docに載せるfieldのregexp)
- しかし、なるべくライブラリなどを使って楽がしたい（？）

## composableなもの

composableではない。 (e.g. jsonschema )

```go
type S struct {
    X X // validation
    Y Y // validation
    Z Z // validation
}
```

composable

```
type S struct {}
func (s S) Fields() []Field {
    return []Field{X(), Y(), Z()}
}
```

いや、type to typeの変換が定義されているならどちらも同じことでは？
なんとなくイメージとしては、domain modelを定義するとinputが浮いてくるみたいなことができるか考えてみたい感じ。

同じでない例を考える事ができる。言語に備わっていないものの定義。

- union (graphql)
- interface (graphql)

(通常はgoの場合はてきとうなinterfaceを切ってテキトーなメソッド（ほぼ空)を定義してマーキングする）

例えば、今作っているapikitのseed部分としてEmailAddress型を生成してみるというのはどうだろう？
(UnmarshallJSONを生成してあげれば自動的にvalidationとして入る。ドキュメントを生成するためのインターフェイスがあれば)

問題は、直接値を作ることが可能な点（まぁいいのではという気もする）。
既存のgithubにあるようなもののコード例に納得がいかないのは、利用者としてのコードだからという気がする？

全部をjson文字列から作るというような感じにすれば良いのでは？
（失敗が実行時になるというのはだるいんだろうか。テストのとき）

### go特有の厄介な話 (oneOf)

goのencoding/jsonはinterfaceのシリアライズ・デシリアライズと相性がとても悪い。
インターフェイスに対応した実装をもたせる事ができない。ということは1つ上の階層のメソッドでいい感じにハンドリングしてあげる必要が出てくる。

### validationをいつやるか？

昔 `UnmarshalJSON()` に押し込めば良いのでは？という話を考えていたが辞めた理由があった記憶があったけれど何だっけ？
(maperrを使うのを辞めた理由、egoistの方ではむしろこれを生成するように頑張ろうとしていた)
これはParse関数の実装を辞めるみたいな話でもある。
エラーの出力をいい感じにできないとか、複数のフィールドのエラーをハンドリングできないとかだっけ？
ネストしたエラーの表現は作ることができたんだよなー。

structの値を直接渡してテストなどで使うことができないとかだっけ？
あー、そうだ思い出した。syntaxとしてinvalid(400)と値としてinvalid(422)を分けることができないじゃん？という気持ちになったからだった。
段階を踏む事ができないのはやっぱり不便だよね？という気持ちになったからだった。

それで明示的にvalidationを呼ぶ実装のほうが良いじゃんと思う様になったのだった。一方で呼び忘れは怖いよね。という話はある。
それでようやくというか傍から見たら振り出しに戻るというような状況になっていくのだった。

### そもそもusecase(struct)はaction(function)で良いという話

これは、routerの設定などにも絡んできちゃうのか？すごく身近な例で言えば、handlerとusecase的なものを同一視したとして

```go
type Server struct {
    DB *DB
}

type (s *Server) Register(...) {
    // ここでs.DBが使える
}

func Mount(r Router) {
    r.Post("/register", s.Register)
}
```

と

```go
func Register(db *DB, ...) {
    // ここでs.DBが使える
}

func Mount(r Router, db *DB) {
    r.Post("/register", Register(DB))
}
```

の違い。

いろいろ考えたけれど、必要となる依存が明示的になるのが後者のメリットかも。
まぁ前者のほうが楽な気もする。使う分にはフィールドを追加するだけで済むし。

例えば、componentとconsumer的な感じで捉えたときに、X,Y,Zというcomponentとf,g,hというconsumerがあるとしたときに、各々のconsumerがどのようなcomponentに依存しているかが後者では明示的になる。
ただ、依存を追加したいときに一個一個引数を追加していく感じなのがだるいかもしれない。