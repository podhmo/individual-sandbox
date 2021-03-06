# [wip]golang mini-grep

- [これ](https://gist.github.com/podhmo/3cd3c1cd8bb1392dafc2eedc07f3cf35) の課題的なもの

mini-grepという名前ですすめてく。

0. とりあえず正規表現を使ったコードを書く。
1. コマンドライン引数を受け取って受け取ったファイルに対してgrep的な処理をする
2. 無引数の場合には標準入力を見る
3. コマンドライン引数でオプションを取れるようにする(-r で再帰的な探索)
4. grepの対象を絞る(--include, --exclude)

とりあえず正規表現を使ったコードを書く。

- regexpを使えば良いっぽい
- stripの方法がわからない -> strings.TrimSpace()

コマンドライン引数を受け取って受け取ったファイルに対してgrep的な処理をする

- コマンドライン引数の取り方がわからない -> os.Args
- ファイルIOの方法がわからない -> os.Open()
- 行ごとの取得の方法がわからない -> io.Readerをbufio.NewScannerに渡す -> その後sc.Scan() + sc.Text()

無引数の場合には標準入力を見る

- これは単にos.Stdinを見るようにすれば良い
- ついでにファイルの存在チェックもすることに -> os.Stat()の結果を見る
- :notebook: そう言えば、switchで分岐できるのだったっけ？
- :notebook: そう言えば、sytax checkerが動いていない。

コマンドライン引数でオプションを取れるようにする(-r で再帰的な探索)

- コマンドラインoptionはflagパッケージを使えば良いはず。
- flag.Bool()を使ってflag.Parse()を呼ぶし。os.Argsの代わりにflag.Args()の結果を使う
- :notebook: そう言えば、高階関数はmini-grepの中ではここで初めて使っている
- :notebook: golintを通すには公開関数にはdocstringを追加しないとだめっぽい。
- :exclamation: golangではclosureのmutual recursionは無理？
- :exclamation: flagパッケージを使って、short optionとlong optionを同時に指定する方法がよくわからない。

grepの対象を絞る(--include, --exclude)

- :notebook: filepath.Globは直接ファイルを探索してしまう

# [wip]golang 正規表現を使う方法

- [regexp - The Go Programming Language](https://golang.org/pkg/regexp/)

```go
import "regexp"


rx := regexp.MustCompile("foo.*")

// defaultは[]byte
rx.Match(byteValue) // => true/false

// stringを使いたければMatchString
rx.MatchString(s) // => true/false
```

他にも必要なものは後でpackageのページを見れば良い。

# [wip]golang ファイルIO

そもそもファイルIOの方法すら忘れてしまっていた。1行ずつ読みだしたりするのにはbufioを使えば良さそう。

```go
// おおよそファイルIOをするときは以下の様な感じ
func doFile(filename string) {
    fp, err := os.Open(filename)
	if err != nil {
		return err
	}
    defer fp.Close()
    doSomethingReader(fp)
}

// 一行ずつ読み取りたかったらScannerが便利
func doSomethingReader(r io.Reader) {
    sc := bufio.NewScanner(r)
    for bufio.Scan() {
        doSomething(bufio.Text())
    }
}
```

- [bufio - The Go Programming Language](https://golang.org/pkg/bufio/#NewScanner)

io/ioutilなどもあるので使い分けやどうすると楽なのかなどは自然と把握できる程度にはなっときたい。

# [wip]golang globパターンでのマッチング

[filepath - The Go Programming Language](https://golang.org/pkg/path/filepath/#Glob)

```go
$ gore
:import path/filepath
filepath.Glob("*.py")
```

ただし文字列の集合に対してglobパターンでマッチングとかはできない。

# [wip] golang コマンドライン引数の取り方

最も単純な方法は os パッケージを使う方法

```go
import "os"

os.Args // 多くの言語と同様にos.Args[0]には自身の名前。os.Args[1:]でコマンドライン引数
```

もう少し柔軟にしたければ flag パッケージを使えば良い

```go
import flag

var recursive = flag.Bool("recursive", false, "search recursively")

// main
flag.Parse()
args := flag.Args() // os.Args[1:] のようなもの

if *recursive {
  DoRecursively()
}
```

- [os - The Go Programming Language](https://golang.org/pkg/os/#Args)
- [flag - The Go Programming Language](https://golang.org/pkg/flag/)

# [wip] golang []byte へのcastでmemory copy

少しだけ興味があった。

- [golang で string を []byte にキャストするとメモリコピーが走ります - Qiita](
- [Goアセンブラ入門 - The Go Programming Language](http://golang-jp.org/doc/asm)


## golangでアセンブリを出力する方法

go toolsの中にあるっぽい。

```go
$ go tool compile -S file.go
# disable optimization ??
$ go tool compile -S -N file.go
```

まだまだ調べることあるので途中
