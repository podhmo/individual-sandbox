## go cli

- https://github.com/cli/cli

コマンドとオプションをtreeにしてみる


- `--help`
- `--version`
- `-R, --repo`
- issue

  - create

    - `-b, --body`
    - `-t, --title`
    - `-w, --web`

  - list

    - `-a, --assignee`
    - `-A, --author`
    - `-L, --limit`
    - `-s, --state`

  - status
  - view

    - `-w, --web`

- pr


- repo

## go cliのhelpの表示

```
Work seamlessly with GitHub from the command line.

USAGE
gh <command> <subcommand> [flags]

CORE COMMANDS
  issue:      Create and view issues
  pr:         Create, view, and checkout pull requests
  repo:       Create, clone, fork, and view repositories

ADDITIONAL COMMANDS
  completion: Generate shell completion scripts
  config:     Set and get gh settings
  help:       Help about any command

FLAGS
      --help              Show help for command
  -R, --repo OWNER/REPO   Select another repository using the OWNER/REPO format
      --version           Show gh version

EXAMPLES
  $ gh issue create
  $ gh repo clone
  $ gh pr checkout 321

LEARN MORE
  Use "gh <command> <subcommand> --help" for more information about a command.
  Read the manual at <http://cli.github.com/manual>

FEEDBACK
  Fill out our feedback form <https://forms.gle/umxd3h31c7aMQFKG7>
  Open an issue using “gh issue create -R cli/cli”
```

## go cli ついにCLIのいい感じのものが思いついた

名前: quick (仮) or gencli

### 使い方のイメージ

```console
$ quick init
# generate quickcli
$ quick init <main package path>:Run
# generate skeleton in quickcli

# generate main()
$ go run quickcli/main.go --inplace <main package path>:Run
# with envvar
$ go run quickcli/main.go --inplace --with-envvar <main package path>:Run
```

go/types呼んだら負けっぽいので、ASTからgo/typesっぽいものを取り出すライブラリが必要そう。
(import先は読み込みたくない。signatureの情報は欲しい)

### quickcli/main.go

quickcli/main.go

```go
// not import
typeRegistry["time.Time"] = TypeInfo{"Path": "<m>.quickcli.time:LookupTime"}

// specific rgistration
namedRegistry[Pair{Type: "time.Time", Name: "now"}] = TypeInfo{"Path": "<m>.quickcli.time:LookupTimeNow"}
```

importしないのが大切。そして、このmain.goは `quick sync` で同期される。
早さのために、importは許さない。なので `quick sync` はastとしてだけ読み込む。

```
func Lookup() context.Contxt {
	return context.Background()
}
```

そして、lookupの定義の単位でコンパイルが通るかは試す事ができる。

### 生成されるmain.go

生成されるmain.goのmain()は、文字列だけに依存する。こうしておくと何が良いかと言うと。

- コマンドライン引数
- 環境変数

この２つを全く意識せず追加できる。

生成されるmain.goは以下のようなものになるイメージ。

```go
// import _time "<m>/quickcli/time"

func main(){
	nowFlag := flag.String("now", "", "<usage>")
	configFlag := flag.String("config", "", "<usage>") // fooはconfigに依存している
	// fooFlag := flag.String("foo", "", "<usage>") // fooは文字列を必要としていない

	flag.Parse()

	// TODO: handle envvar
	// TODO: required check

	run := func() error {
		ctx := context.Background()
        now := _time.Lookup(quickcli.Name(*nowFlag))
		config := _config.Lookup(quickcli.Name(*configFlag))
		foo, err := _foo.Lookup(config)
		if err != nil {
			return fmt.Errorf("lookup Foo, but, %w", err)
		}
		return Run(ctx, now, foo)
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
    }
}

// 以下のような関数があるイメージ
// func Run(ctx context.Context, now time.Time, foo Foo){ }
```


### lookup関数の定義

どういうバリエーションがあるんだろう？ 

引数はこんな感じ？

```
type Name string

func (name quick.NeedFlag, ...)
func (ctx context.Context, ...)
func (ctx context.Context, name quick.NeedFlag, ...)
```

戻り値はこんな感じ？

```
func (...) <Ob>
func (...) (<Ob>, error)
func (...) (<Ob>, func())
func (...) (<Ob>, error func())
```

### default値

LookupXXXのdefault値はあればDefaultXXXで良いのでは？
flagの値が空だったら、DefaultXXXが呼ばれれば良い。

というか、空文字だったらの操作で良いのでは？

### scan

以下のような機能もあると便利そう。

- scan 関数のsignatureから情報を取り出すだけ
- dump-context 関数のsignatureと利用した定義から依存解決した結果を返すだけ
- emit (コード生成)

### run-time context

実行時に渡されたファイル名を取り出したい。cliの名前を取るのに。

```go
rc := quick.GetRuntimeContext()
rc.Path // xxx/cmd/foo/main.go:Run
rc.Filename // xxx/cmd/foo/main.go
```

### 同じ型の値を利用したい

これは引数が異なれば良いと言うイメージ。
引数名とフラグ名は同一視しても良いのではないか？ (needFlagの引数名)

特定の型の特定の場所を使いたいとかはどうすれば良いんだろうね。。

- foo.goはconfig.fooを見る
- bar.goはconfig.barを見る

みたいなやつ。reflectを使って取り出せば良いのでは？（今は困っていない）

## marshmallow with dataclasses

marshmallowを上手くdataclassesでも期待通りに動かすのが地味にだるいな。

### marshmallow_dataclass

https://github.com/lovasoa/marshmallow_dataclass

- hmm `from __future__ import annotations` is broken
- https://github.com/lovasoa/marshmallow_dataclass/blob/master/marshmallow_dataclass/__init__.py#L589

NewTypeの実装ありかもしれない？ -> pluginも実装されてた。

## hmm typeguard?

- https://github.com/agronholm/typeguard
