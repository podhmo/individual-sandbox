#[golang][kingpin][memo] 最近goでCLIのコマンドを作るときの雛形のメモ

:warning: この記事は自分の書いた[ブログ](http://pod.hatenablog.com/entry/2018/03/17/150717) からの転載です。他にこうしたら良いだとかのアドバイスやコメントなどがあったら教えてもらえると嬉しいです。


goでCLIのコマンドを作る時にたまにどう書くか忘れて調べたりするので自分用の雛形をメモしておく。

## kingpin

だいたい自分でCLIのコマンドを作る場合には以下のようにしている。

使っているのは[kingpin](https://github.com/alecthomas/kingpin)。とは言えこのパッケージにこだわりがあるというわけではなく。goのコマンドラインパーサーのパッケージはとても量が豊富ですべてを試す気にはならないというのと。自分が使っている範囲では必要十分なので他に移行する気がおきないという消極的な理由。

`--foo` みたいな形式のlong optionには対応したかったのでflagではなく外部のパッケージを使っているところがある。

## 雛形

雛形は以下のようなもの。 語るべきところは色々あるのでコメントで番号を振ってみる。

```go
package main

import (
	"log"
	"os"

	"github.com/pkg/errors"
	"gopkg.in/alecthomas/kingpin.v2"
)

// 1
type opt struct {
	path   string
	dryrun bool
}

func main() {
	var opt opt
	app := kingpin.New("app", "my app")

    // 1
	app.Flag("dryrun", "dry run option").BoolVar(&opt.dryrun)
	app.Arg("path", "target file path").Required().ExistingFileVar(&opt.path)

    // 2
	if _, err := app.Parse(os.Args[1:]); err != nil {
		app.FatalUsage(err.Error())
	}

    // 3
	if err := run(opt.path, opt.dryrun); err != nil {
		// 4.
		log.Fatalf("%+v", err)
	}
}

func run(path string, dryrun bool) error {
	return errors.New("hmm")
}
```

##  1. オプションはバラバラに定義するよりstructにまとめたい

オプションはバラバラに定義するよりstructにまとめたい。なので幾つかのkingpinの例やflagの例とは異なり、トップレベルでvarを定義したりだとかローカル変数経由でflagやargを指定したりしていない。その代わりに、作りたいコマンドのオプションに対応するstructを定義してそこにすべての引数を束縛している。

同様の理由で`String()` ではなく `StringVar()` を使っている。

## 2. parseエラーになったときにはusageを表示したい

parseエラーになったときにはusageを表示したい。最初の頃は雑にpanicを使っていたのだけれど。しばらく使っていなかったコマンドを使おうとした時に対応していないオプションなどを指定してしまうことが多かった。その度にpanicが起きるというのは精神衛生上あんまり良い体験ではなかった。加えてその後必ず `--help` 付きで実行してヘルプメッセージを確認する形になる。どちらもまとめてやったほうが良い。

ちょっと調べたら `FatalUsage()` というメソッドがあったのでこちらを使うべきという結論になった。

たとえば、panicのときには以下のようなメッセージになる。

```console
panic: required argument 'path' not provided

goroutine 1 [running]:
main.main()
        /home/podhmo/sandbox/example_kingpin/00app/main.go:24 +0x544
exit status 2
```

これが `FatalUsage()` を使うと以下の様なメッセージになる。

```console
$ go run main.go 
app: error: required argument 'path' not provided
usage: app [<flags>] <path>

my app

Flags:
  --help    Show context-sensitive help (also try --help-long and --help-man).
  --dryrun  dry run option

Args:
  <path>  target file path

exit status 1
```

デフォルトでは `-h` を `--help` と解釈しないので、誤って `-h` を入力してヘルプメッセージを見ようとしてパニックしていらいらを募らせるというようなこともなくなったので良い。

## 3. mainとrunに関数を分けている

mainとrunに関数を分けている。これは実際のCLIのコマンド本体とは関連がないかもしれない。分けている理由は1つでエラーの取扱が楽になる点。デフォルトでは上に書いたコードのように `log.Fatal` で包むだけなのだけれど。時折違うような対応をしたくなることがある。その時に関数をmainだけにして、mainとrunに分けていないと全部のエラーハンドリング部分を治す必要が出てくる。

例えば分けていない場合にはトップレベルの関数(main)のコードが以下の様になってしまうので。

```go
if err := f(); err != nil {
	log.Fatal(err)
}
if err := g(); err != nil {
	log.Fatal(err)
}
if err := h(); err != nil {
	log.Fatal(err)
}
```

かと言ってエラーを変数に代入していくみたいなコードも気持ち悪い。それに、終了ステータスを気にする処理はmainで、内部で呼ばれるrunはただのerrorを返す関数という形にした方が収まりが良い。

## 4. デフォルトのエラーハンドリングにはlog.Fatalではなくlog.Fatalfを使っている

デフォルトのエラーハンドリングにはlog.Fatalではなくlog.Fatalfを使っている。これはほとんどpkg/errorsの対応のため。

個人的にはスタックトレースが大好きなのでpanicではないエラーであってもスタックトレースが見たい。適切なエラーハンドリングができている場合にはスタックトレースなども不要という意見があるのもわかるけれど。適切でないエラーハンドリングのコードであればあるほど不用意なエラーに遭遇することが多い。そのようなものほどスタックトレースが見たくなる。加えて不慣れなところで作業するときには不慣れな語彙の理解の元作業することが多く。そのような場合には少しでも情報量がほしい。そんなわけでスタックトレースは欲しい。

幸い、pkg/errors経由のエラー値は "%+v" で出力した場合にはスタックトレースも表示してくれる(まじめに話すとFormat()メソッドのフラグの扱いなのだけれど。詳しくはfmtあたりのドキュメントを見てください)。なのでlog.Fatalではなくlog.Fatalfで "%+v" を指定して使っている。

## おわりに

現状使っているのはkingpinなのですが。すべてのgoのコマンドラインパーサーのパッケージを試したわけではありません。他にこうしたら良いだとかのアドバイスやコメントなどがあったら教えてもらえると嬉しいです。
