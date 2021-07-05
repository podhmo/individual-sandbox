# go structopt

goのCLIの定義をstructの設定だけに限定してみればべんりなのではないか？
難しく考えすぎない。

## concept

- 既存のライブラリは覚える事が多すぎる
- ほとんどstructとタグで定義する。実はCLI用の特別な型毎の対応って不要なんじゃないか？

  - 型の変換はjson.Unmarshallerにやってもらう
  - validationは他のライブラリに任せる
  - ネストした表現は `--foo.bar` 的な形で指定可能
  - parseしかしないのでreflectを使っても構わないのでは？


その他CLI parserとして必要なもの

- required/unrequiredの指定
- ヘルプメッセージもそれなりに自動生成e
- flagではなくpositional argumentsの指定
- 環境変数での設定は必要
- デフォルト値も必要

あれば良いもの

- 無限にネストしたサブコマンド
- ベース部分で共通したサブコマンドのオプションを定義 `<command> --base-option <subcommand> --sub-option`
- (ヘルプメッセージのようなドキュメント生成（後回し。やらないかも）)
- (補完は生成できると嬉しい？（後回し。やらないかも）)
- (色付きでキレイな出力のほうが嬉しい？（後回し。やらないかも）)

## 実行例

```go
package main

import (
    "os"
    "github.com/podhmo/structopt"
)

type Option struct {
    Port int `json:"port"`
}

// cmd --port 44444

func main(){
    var opt Option
    p := structopt.NewParser(&opt)
    if err := p.Parse(os.Args[1:]); err != nil {
        p.ShowHelp(err)
        return os.Exit(1)
    }
}
```

ネストした表現

```go
type Option struct {
    AppConfig struct {
        Port int `json:"port"`
    } `json:"app"`
    DBConfig DBConfig `json:"db,omitempty"`
}

type DBConfig struct {
    URI string `json:"uri`
}

// cmd --app.port=44444 --db.uri sqlite:///xxx.db
```

## 細かな話

### 環境変数の対応

flagパッケージで手軽に全体に環境変数でも設定できるようにする対応がめちゃくちゃ便利。
ただ、型をどうするかという悩みが出てきそう。
int,floatなどのセット以外は全てstringとしたJSONを一度生成してあげれば良いのでは？

優先度的には以下の様な感じ (コマンドライン引数が最優先)

1. コマンドライン引数
2. 環境変数
3. デフォルト値

### エラーの扱い

ECSなどで実行するときにParseErrorもJSONでログを出したい場合があるかも。
引数のparseのerrorとアプリケーションの実行のerrorは分けたい気がする。

通常の利用では以下の様になってほしい

- parseError -> help messageの表示
- applicationError -> `log.Fatalf("!! %+v", err)` 的な表示

applicationErrorのことは考えたくない。ライブラリの適用範囲外になるのでは？

### サブコマンドの扱い

これも複雑なことを考えたくない。可能なら構造をあまり意識せず自由にサブコマンドをつけたい。
WAFのrouterの設定と同じような形で設置できるのが良いのではないか？つまるところmountする感じ

```go
// single route
fooParser = structflag.NewParser(&opt)
fooParesr.Parse(os.Args[1])

// multi route (sub-command)
r := structflag.NewRouter()
r.Mount("foo", fooParser)
r.Mount("bar", barParser)

// more nested
root := structflag.NewRounter()
root.Mount("web", r)

// cmd web foo --port=44444
```

メソッド名を分けたほうが明示的？ (e.g. AddParser, AddRouter)

#### 共通オプション

サブコマンドの共通オプションをベース部分で設定したい場合はどうする？（要検討事項）
埋め込みを使って定義できれば良いんじゃないか？

```go
type BaseOpt struct {
    Debug bool `json:"debug"`
}

type FooOpt struct {
    BaseOpt
    Port int `json:"int"`
}
```

こうしてしまうと、依存した共通オプションを扱うコマンドが固定できない。とはいえあんまり真面目に個別に設定したりしたくないのだよなー。

```console
$ cmd --debug foo --port
```

この辺が無限にネストしたサブコマンドとの対応でめんどくさい話になる？
Routerに持たせれば良いのでは？(いい感じに上位のオプションが伝搬してくれないと困りそうだけれど)

（解決方法が決まっていない）

### positional arguments

positional argumentsはどうやって扱えば良いんだろうか？
基本的には、フラグ以外をどう扱うかという話なのだけれど、struct中でどうやって扱うか？

通常、フラグの場合はこういう感じ。

```go
type Opt struct {
    Name string `json:"name"`
}
```

これは、web APIでquery stringやpathの値を束縛するのと同じ感じか。

- ginはstructではない https://github.com/gin-gonic/gin#querystring-parameters
- echoも `QueryParam()` とか https://echo.labstack.com/guide/request/
- fiberも `Params()` で取り出す  https://docs.gofiber.io/guide/routing#parameters

JSONに合わせてしまいたい様な気もする。フィールド以外を `-` で表せるような気もしたけれど。
除外という意味の方が強いような気がする。でも、タグの情報を増やしたくないなーと思いつつ。

ただ、ヘルプメッセージを表示するためにタグに何か書くか、そう考えると、普通にもう一つ追加して良い気がする。

kindはでもなんかわかりづらいような気がする。でもflagパッケージはpositional argumentsに対応していないから `-` が自然な気がする。 `flag.Args()` で残りが取り出せて `flag.NArg()`, `flag.NFlag()` でそれぞれの個数が分かる感じ。

```go
type Opt struct {
    Name string `json:"-" description:"target of greeting"`
    Rest []string `json:"-"`
}

// cmd foo bar boo
// -> opt.Name == "foo"
// -> opt.Rest == ["bar, "boo"]
```

### デフォルト値の利用

デフォルト値はjson.Unmarshallerに渡す値の初期化時に渡せば良いのでは？
ネストしたときの挙動がちょっと気持ち悪い感じになりそう？mergoとか使えばなんとかなるんじゃ。

- https://github.com/imdario/mergo

```go
var opt = Option{Port: 44444}
p := structopt.NewParser(opt)
p.Parse(sys.Args[1])
```

### ヘルプメッセージの表示

これは今の所２種類の方法が考えられる。

- Parserという単位で設定値を持つことにして、`p.ShowHelp()`みたいなメソッドを呼ぶ
- 全ての情報にアクセス可能なParseErrorを返す

実は後者のほうが良い説がある

### ヘルプメッセージのカスタマイズ

ヘルプメッセージの変更はどうしよう？オプション事の説明自体はタグに書けば十分だとして、prelude,epilogみたいな部分。

- structにコメントを書いても取れない。
- 素直に関数を渡せるようにしよう

```go
p := structopt.NewParser(&opt)
p.HelpFunc = func(o io.Writer, help func() string) erorr{
    content := help()
    fmt.Printf(o, `
prelude

%s

epilog
`, content)
    return nil
}
```

この辺はflagパッケージに寄せる感じにできる気はする。

#### デフォルト値をヘルプメッセージに含めたい
#### 環境変数の情報をヘルプメッセージに含めたい

### debug

タグで指定したときにめんどくさいのはうまく動いてなさそうな時に調べること。
これはparseとrunが分かれていればよいというだけなので `FAKE_CALL=1` とかしたらParse時点で終了してあげれば良いのでは？JSONを出力して。

```
// first step (デフォルト値)
{}
// second step (コマンドライン引数をJSONにした値)
{}
// result (parse結果)
```

実際の実行はせずに止まる。

### ショートハンド的な物もあったほうが良い？

シンプルな表現で書きたくなることがある。

```go
package main

import (
    "github.com/podhmo/structopt"
)
type Option struct {
    Port int `json:"port"`
}

// cmd --port 44444

func main(){
    var opt Option
    if err := structopt.Parse(&opt); err != nil {
        structopt.ShowHelpAndExit(err)
    }
}
```

#### argopt

いっそのこと、こうしてしまいたい。関数の引数定義からstructを生成。
(ただ、reflect時に引数名の情報が取れないのでこの方法は破綻しているかも。ASTからとってくることはできるがそれはバイナリをビルドして使う形になった瞬間に死ぬ)

```go
package main

import (
    "github.com/podhmo/structopt/argopt"
)

func Run(port int) error {
    doSomething(port)
    return nil
}

func main(){
    if err := argopt.Run(Run); err != nil {
        structopt.ShowHelpANdExit(err)
    }
}
```

引数をまとめた表現も使える。

```go
func run(opt struct {Port int `json:"port"`}) error {
    ...
}
```

うーん、でもこんなことをするなら素直に型を書くのでは？デフォルト引数にも対応できないし。
argoptは無しの方向で。