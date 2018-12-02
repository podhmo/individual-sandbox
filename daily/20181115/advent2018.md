#[go]go runの前後に処理を挟んで実行したい

## はじめに

この記事は[Goアドベントカレンダー]の５日目の記事です。

## go runの前後に処理を挟むとは？

go runの前後に処理を挟むとは通常以下の様なコードである部分を

```go
func main() {
	..
}
```

以下の様に変えることです。

```go
func main() {
	doSomethingBeforeMain() // deferが付く場合もある
	mainInner() // 元のmain()
	doSomethingAfterMain()
}

// 元のmain()
func mainInner() {
	..
}
```

これをどうやるのが良いのかなどを考えたりしました。

## もう少していねいな導入

もう少しgo runの前後に処理をはさみたいという動機について話します。

### URLアクセスの挙動を少しだけ変更したい

goで書くことに慣れてくるけっこう何でもgoで書きたくなるときがあります。通常のgoの用途ということであればシングルバイナリということで何某かのツールを作ってビルドと言うことが多いですが、goが手に馴染んでくると共にちょっとした処理もgoで書いてしまって、スクリプト感覚で `go run` を呼び出すというようなことをしたくなります。

```console
$ go run daily/scripts/xxxx/main.go <option>
```

時間の経過と共にそのような書き捨てのスクリプトが徐々に溜まっていきます。

さて、そのような書き捨ての便利スクリプトのうちの１つの動作が特定のURLにリクエストするようなものだったとします。そしてそのスクリプトがある日失敗します。

```
$ go run daily/scripts/xxxx/main.go
2018/11/29 01:42:28 Get https://example.com: x509: certificate has expired or is not yet valid
exit status 1
```

例えば不正なSSL証明書であった場合にはエラーになります(たまたまちょうどこの記事を書いている最中にexample.comの証明書が期限切れだったので例にあげました。今は直っているようです)。あんまり良い素行とは言えませんが、今回に限り動けば良いアクセス先は既知のものという場合には手抜きをして[証明書のチェックを無効に](https://stackoverflow.com/questions/12122159/how-to-do-a-https-request-with-bad-certificate)にしてしまいたくなります。

例えば、以下の様なコードを追加してあげるとDefaultTransportを使う処理に関しては、SSLの検証を無効にします。

```go
http.DefaultTransport.(*http.Transport).TLSClientConfig = &tls.Config{InsecureSkipVerify: true}
```

### 内部で呼ばれている通信をtraceしたい

今度は逆に他者が書いたコードを実行する機会があるとします。そのコードはなにやら実行時にエラーを返しているようですが原因がわかりません。丁寧にエラーハンドリングされているようなコードであればエラーメッセージを見れば対応方法が分かるものの、えてしてエラーを返しやすいコードというのはエラーハンドリングが雑であることが多かったりします(例えば外部との通信が常に成功するという前提に立ったようなコードなど)。

```console
$ go run another/cmd/yyyy/main.go
panic // anything wrong?
```

何やらサードパーティのライブラリを使って通信しているようですが、どのようなAPIにrequestしているかわかりません。まじめにライブラリなどの実装を追えば分かるのかもしれませんが、あんまり深入りしたくはありません。

手前味噌ではあるのですが、昔に[go-traceable](https://github.com/podhmo/go-traceable/tree/master/httptrace#patch)というパッケージを作っていたことがありました。このパッケージは完成品ではないので実用するのはオススメしないのですが、このパッケージの`httptrace.Patch()`を実行すると通信内容を[net/http/httputil](https://golang.org/pkg/net/http/httputil/)の関数を使ってdumpしてくれます。

TRACEという環境変数を渡してあげると、requestをtraceします(traceされたファイルがどのようなものか気になる方は[gist](https://gist.github.com/podhmo/6a297701071389105ae523e7cc1ec504)を参照)。

例えば、以下は[googleのspreadsheetのAPIのquickstartの例](https://developers.google.com/sheets/api/quickstart/go)をtraceしたときの例です。

```
$ mkdir -p output
$ TRACE=output go run main.go
2018/12/02 18:27:48 trace to 0001https:@@sheets.googleapis.com@v4@spreadsheets@1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms@values@Class%20Data%21A2%3AE?alt=json&prettyPrint=false
...
Will, Math

$ tree output/
output/
└── 0001https:@@sheets.googleapis.com@v4@spreadsheets@1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms@values@Class%20Data%21A2%3AE?alt=json&prettyPrint=false

0 directories, 1 file
```

部分的にではあるのですが、HTTPSの通信に対してもMITMを仕掛ける必要もなかったり、http.DefaultClientを尊重しているライブラリであればミドルウェアとの通信内容も見れたりで便利です(一例としては、Elasticsearchの[olivere/elastic](https://github.com/olivere/elastic))。

## 実行前に中のコードを触りたくない

何かの機能が既にコード上に組み込まれていれるなら、環境変数などで指定することで実行時の挙動を便利に変更できるのですが(`DEBUG=1`などが良い例です)、先程の例などのように元々の作者が期待したものとは少し違う振る舞いのを追加しようとした場合にはコードを書き換える必要があります。慣れればこれらも惰性での作業にはなるのですがやっぱり面倒です。

自分の手でエディタを開いて変更を加えてから実行するという操作以外でこれらのことができないか考えてみることにしました。

### 出力されたバイナリをいじる

記事のタイトルがgo runになっている通り、ビルドしたバイナリをいじるというのは検討段階ですぐ諦めました。(Linux環境での場合に限り)`LD_PRELOAD`みたいな何かを使って黒魔術的なことができないかとか、バイナリの関数部分の番地を書き換える([bouk/monkey](https://github.com/bouk/monkey)みたいな方法もありますが、ちょっとそこまで頑張る気力がありませんでした（もし方法があるのだとしたらもう少し考えてみたいです）。

### go runの対象を作る

とりあえず

## 実際にコマンドとして組み込むにはどのようにパッケージングしたら良いか？

次にgo runの対象を作るということをしているプロジェクトは無いかと考えてみることにしました。そのときの記憶の中で引っかかったのは以下２つです。

- [goa](https://github.com/goadesign/goa)
- [mage](https://github.com/magefile/mage)

それぞれについてざっくりとどのようにしているかここにまとめてみます。

### goa

https://goa.design/

> goa is a holistic approach for building microservices in Go.

goaはgoで書かれたDSL(設定ファイルのようなイメージ)から、application serverに必要となりそうな諸々のコードを生成するコマンドを提供するプロジェクトです。現在stableなのはv1ですが、masterがv2の方を向くようにされようとしていたりv2が後々主流になっていくのではないか状況です。

goaは実は`main()`のコードは生成していますが、go runの対象を作ってはいません。`goa gen`でコード生成が行われるのですが、`-debug`付きで実行するとコード生成に利用した`main()`のコードが残ります。実行時には`goa<suffix>`というディレクトリにコード生成用の`main()`が生成され、`goa`という名前のバイナリがビルドされます。

```console
# ./gen/以下に各種コードが出力される
$ goa gen github.com/goadesign/goa/examples/calc/design --debug

$ ls goa538526107/
├── goa
└── main.go

0 directories, 2 files
```

まとめると以下の通りです

1. `./goa<suffix>`というtmp directoryに実行用のコードを出力
1. tmp directory上に`goa`という名前のバイナリを出力
1. `goa`バイナリを実行 `gen` 以下にコード生成
1. (tmp directoryを削除)

### mage

https://magefile.org/

> Mage is a make/rake-like build tool using Go. You write plain-old go functions, and Mage automatically uses them as Makefile-like runnable targets.

mageはmakeのgo版をイメージしてもらえれば良いです。Makefileの代わりにGoの通常の関数を書いていきます。mageの方は`+build ignore`の付いた`main()`のコードを生成して利用します。 `-keep`オプションを付けると生成した`main()`を含んだファイル(mage_output_file.go)は残ったままになります。

```console
$ ls
magefile.go
$ mage ls -keep
mage_output_file.go  magefile.go
$ ls
mage_output_file.go  magefile.go
```

実はmageもまた`go run`自体は使っていません。goa同様にバイナリを出力してそれを利用しています。具体的には`/home/<name>/.magefile/5eb48aa7125cbb7d7a85a15ad4af2e980b1a3107`のような位置にバイナリを出力して、それを実行します。実際の挙動は以下です。

1. `./mage_output_file.go`を出力
1. `$HOME/.magefile/<hash>`にバイナリを出力
1. (`./mage_output_file.go`の削除)

ちなみに`main()`のコード生成自体は[text/template](https://golang.org/pkg/text/template/)を使った素朴なもので特にむずかしいことはしていませんでした。
