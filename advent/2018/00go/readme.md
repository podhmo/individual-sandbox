#[go]go runの実行をwrapしてhttp/httpsのrequestを手軽にtraceしたい

## はじめに

この記事は[Goアドベントカレンダー]の５日目の記事です。

### 自己紹介

ちょっとだけ自己紹介を。好きな標準ライブラリはgo/astやgo/typesです。愛憎半ば的なライブラリは[x/tools/go/loader](https://godoc.org/golang.org/x/tools/go/loader)です。今年は[gomvpkgのlight版](https://github.com/podhmo/gomvpkg-light)を作ったりしてました。

## ちょっとした導入

### 溜まっていく書き捨てのコードたち

goで書くことに慣れてくるけっこう何でもgoで書きたくなるときがあります。通常のgoの用途ということであればシングルバイナリということで何某かのツールを作ってビルドと言うことが多いですが、goが手に馴染んでくると共にちょっとした処理もgoで書いてしまって、スクリプト感覚で `go run` を呼び出すというようなことをしたくなります。

```console
$ go run daily/scripts/xxxx/main.go -c <config>
```

時間の経過と共にそのような書き捨てのコードが徐々に溜まっていきます。

...と、言う風に書かれたコードたちありました。

### 初めて触ったコードでエラー

新しい人が途中からプロジェクトに参加しました（今までコードを書き溜めてきた人と違う人が操作していると思ってください）。

その人は、内部のコードについては不案内で、どのように書かれているかは把握していません。しかしとりあえずは存在するドキュメントを見ながら書かれた通りの引数を渡してこのスクリプトのようなものを実行しているようです。そのような書き捨ての便利スクリプトのうちの１つの動作が特定のURLにリクエストするようなものだったようでした。

そのコードがある日失敗します。

```console
$ go run daily/scripts/xxxx/main.go -c <config>
failed (status 400). // panicですらないのでstack traceもなし
```

なにやら実行時にエラーを返しているようですが原因がわかりません。どこかにrequestをしていて400ということだけが分かるようです（よりひどいのはcontext canceledとだけ書かれたメッセージでしょうか?)。

### 丁寧なコードであればエラーメッセージから対処の仕方が分かるはずだけれど

丁寧にログが出力されているようなコード、丁寧にエラーハンドリングされているようなコードであれば、エラーメッセージを見れば対処の仕方が分かるものの、えてしてエラーを返しやすいコードというのはエラーハンドリングが雑であることが多かったりします（なにぶん出自は書き捨てのコードから始まっていました）。

例えば外部との通信が常に成功するという前提に立ったようなコードなどstatusが200である以外の処理が雑であったりします(はじめから丁寧なコードをというお叱りは真摯に受け止め善処したいというような声)。

今回のエラーに関して言えば、何やらサードパーティのライブラリを使って通信しているようですが、どのようなAPIにどのようなrequestを投げているかわかりません。まじめに中のコードやライブラリの実装を追えば分かるのかもしれませんが、あまり深入りしたくはありません。

このような時に、requestをtraceできると便利です(長い導入のおしまい)。

## requestのtrace

requestのtraceと言っているのは以下の様なイメージのものです(実行時の通信をキャプチャしてその時のrequest,responseを覗き見したいということ)。

```console
$ daily/scripts/xxxx/main.go <何か特殊なオプション> -c <config>

# 何やらrequestされたURLが分かる
request https://examples/xxx/yyy?v=1
request https://examples/xxx/zzz?v=2
request https://examples/xxx/xxx?v=1

$ ls <どこかのディレクトリ>
0000:https://examples/xxx/yyy?v=1
0001:https://examples/xxx/zzz?v=2
0002:https://examples/xxx/xxx?v=1

$ cat <どこかのディレクトリ>/0000:https://examples/xxx/yyy?v=1

URL <URLの表示>
<request header>
<request body>
<status>
<response header>
<respnose body>
```

とりあえずは、[net/http](https://golang.org/pkg/net/http)のDefaultClientのTransport(RoundTripper)をいじってあげると望みの機能をもたせることができそうです。詳しくは調べて見てほしいのですが、requestの前後で処理を間に挟むというようなことができます。

この方針の便利な点は、対応が部分的になる可能性があるのですが、HTTPSの通信に対してもMITMを仕掛ける必要がなかったり、http.DefaultClientを尊重しているライブラリであればミドルウェアとの通信内容も見れたりで便利です(一例としては、Elasticsearchの[olivere/elastic](https://github.com/olivere/elastic))。そしてgoのエコシステムとしては意外と治安が良いのか(?)それなりに多くのライブラリがhttp.DefaultClientを尊重しているようです。

## trace機能を追加したい(httptrace)

手前味噌ではあるのですが、昔に[go-traceable](https://github.com/podhmo/go-traceable/tree/master/httptrace#patch)というパッケージを作っていたことがありました。このパッケージは完成品ではないので実用するのはオススメしないのですが、このパッケージの`httptrace.Patch()`を実行すると通信内容を[net/http/httputil](https://golang.org/pkg/net/http/httputil/)の関数を使ってdumpしてくれます。

### hello world

簡単な利用例を示します(readmeから持ってきました)。https://examples.net にアクセスしているだけのコードです。

```go
package main

import (
	"fmt"
	"net/http"
	"os"

	"github.com/podhmo/go-traceable/httptrace"
)

func main() {
    // 事前に実行
	teardown := httptrace.Patch()
	defer teardown()

    // ここのrequestがtraceされる
	resp, err := http.Get("https://example.com")
	if err != nil {
		panic(err)
	}
	fmt.Println(resp.Status)
}
```

実行時にTRACEという環境変数を渡してあげると、requestをtraceします。

```console
$ TRACE=1 go run main.go
GET / HTTP/1.1
Host: example.com
User-Agent: Go-http-client/1.1
Accept-Encoding: gzip

HTTP/2.0 200 OK
Accept-Ranges: bytes
Cache-Control: max-age=604800
Content-Type: text/html
Date: Tue, 29 May 2018 22:26:06 GMT
Etag: "1541025663"
Expires: Tue, 05 Jun 2018 22:26:06 GMT
Last-Modified: Fri, 09 Aug 2013 23:54:35 GMT
Server: ECS (sjc/4E8D)
Vary: Accept-Encoding
X-Cache: HIT

<!doctype html>
<html>
...

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is established to be used for illustrative examples in documents. You may use this
    domain in examples without prior coordination or asking for permission.</p>
    <p><a href="http://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
200 OK
```

正確にいうと、`TRACE=xxxx`という環境変数について以下の様な振る舞いをします。

- xxxxが存在しない場合、標準エラーにtrace内容を出力
- xxxxが存在する場合、xxxxディレクトリにtrace内容を出力

HTTPSの通信に対してもMITM用のproxyを立ててそのproxy越しに通信などせずとも済むので便利です。便利なので２回言いました。

#### advanced (google api)

また、http.DefaultClientを尊重しているライブラリであれば、ライブラリを通したrequestに関してもtraceが効きます。そのようなライブラリは幾つかあるのですが、そのうちの１つはgoogle apiのライブラリです。

例えば、以下は[googleのspreadsheetのAPIのquickstartの例](https://developers.google.com/sheets/api/quickstart/go)をtraceしたときの例です。

```
mkdir -p output
TRACE=output run main.go
2018/12/04 01:51:50 trace to 0001https:@@sheets.googleapis.com@v4@spreadsheets@1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms@values@Class%20Data%21A2%3AE?alt=json&prettyPrint=false
Name, Major:
Alexandra, English
Andrew, Math
Anna, English
Becky, Art
Benjamin, English
Carl, Art
...
Robert, English
Sean, Physics
Stacy, Math
Thomas, Art
Will, Math

$ tree output/
output/
└── 0001https:@@sheets.googleapis.com@v4@spreadsheets@1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms@values@Class%20Data%21A2%3AE?alt=json&prettyPrint=false

0 directories, 1 file
```

traceされたファイルがどのようなものか気になる方は[gist](https://gist.github.com/podhmo/6a297701071389105ae523e7cc1ec504)を参照してみてください(access tokenの部分だけはマスクしてあります)。

## 実行前に中のコードを触りたくない

:warning: ただしここで注意点があります。元々の出自は書き捨てのコードでした。

何らかの機能が既にコード上に組み込まれていれるなら、環境変数などで指定したりオプションを指定したりすることで実行時の挙動を便利に変更できるのですが(`DEBUG=1`などが良い例です)、そもそも今回の対象は深入りしたくない書き捨てが出自のコードでした。デバッグ時のための便利な機能など持っているはずがありません。

また、「全てのコードに対してデバッグしやすくするためにxxx用のコードを追加しておくべき」みたいなルールを決めたりするのは些か面倒です(政治的な活動は、好きな人と嫌いな人、得意な人と苦手な人がいます)。必要であればやるべきですが。

冒頭でエラーに遭遇してしまった人のことを思い出してみてください。このプロジェクトに関しては新顔でした。この種の便利な機能に対して、はじめから周囲に自信満々でこれをやるべきなどとコミュニケーションをするより、黙って静かに作ってみた機能が動作するかを試してみたいと思いませんか？(気質の問題かもしれません)。

先程の`httptrace.Patch`は一種のモンキーパッチのようなもので、手元のmainのコードに少しのコードを追加すれば良いだけではあるのですが、めんどくさかったりします(怠惰なんです。生まれの問題かもしれません)。

そこで、今回[go-run-httptrace](https://github.com/podhmo/go-traceable)というコマンドを用意しました。これを`go run`のように使ってもらえるとhttputilによるtrace付きで実行されることになります（ただしgo runとしては機能はサブセットです複数の*.goを指定やパッケージを指定しての実行はサポートしていません）。

例えば、これはexamplesにある[GithubのAPIを叩いたコード](https://github.com/podhmo/go-traceable/blob/master/httptrace/_example/github/main.go)の実行の例です。

```console
$ mkdir -p output
$ TRACE=output go-run-httptrace httptrace/_example/github/main.go
2018/12/02 22:09:01 parse httptrace/_example/github/main.go
2018/12/02 22:09:01 transform AST
2018/12/02 22:09:01 format
running via github.com/podhmo/go-traceable/cmd/go-run-httptrace/
2018/12/02 22:09:02 trace to 0001https:@@api.github.com@repos@podhmo@go-traceable@contributors
2018/12/02 22:09:03 rollback httptrace/_example/github/main.go
$ tree output
output
└── 0001https:@@api.github.com@repos@podhmo@go-traceable@contributors

0 directories, 1 file
```

traceされたファイルがどのようなものか気になる方は[gist](https://gist.github.com/podhmo/18440767785862237e3b73850ddfb7ba)を参照してみてください。

### 実装について

内部の実装についてはひどく単純で、go runの前後に処理を挟むようにASTの変換をしているだけです。

前後に処理を挟むとは通常以下の様なコードである部分を

```go
func main() {
	..
}
```

以下の様に変えるということです。

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

この状態のコードをgo runで実行し実行後に元のコードに戻しています。

## おわりに

「go runの実行をwrapしてhttp/httpsのrequestを手軽にtraceしたい」ということで、以下の２つでhttpsにも対応したtraceの機能を作って紹介してみました。

- DefaultTransportにパッチを当てるコードの追加
- go runされるmain()をAST変換

最初は「go runの前後に処理を挟んで実行したい」という題で記事を書こうかと思ったのですが、内部の実装についてくわしく書くよりも何か便利なツールを作りその紹介の方がわかりやすいかなーと思い今回の記事になりました。なので実際には「main()の前後に処理を差し込む」という機構を使って何か面白いことができないか？ということに興味があったりします。何か面白いことを思いついた人は教えてくれたら嬉しいです。

試しに作ってみたgo-run-httptraceについてですが、手軽さという意味で内部でgo runを実行しちゃってます。しかし、[mage](https://magefile.org/)や[goa](https://goa.design/)の実装を見ているとgo buildでバイナリを生成してそのバイナリを実行という風になっているのでそちらのほうが良いかもしれません(実行前に変換したコードを元に戻せます)。go run自体も内部ではtmp directoryにbuildして実行していたりしますね。ちなみにinternal packageなどの扱いを考えての手抜きで元のコード自体を変換して上書きするという形式を取っています(もっと良い手順があればそちらに移行したいです)。

あと、実装しやすかったからということでhttputilのdumpの機能を使っていますが、みやすさなどを考えると他の表現の方が見やすいかもしれません。あるいはブラウザ上でリクエストを記録したときなどのように.harの形式でまとめても良かったかもしれません。その他traceという話で言えば、手軽にMITMのproxyを立てるツールキットのようなものを作った方が良いのかもしれません。

### ほんとにさいごに

個人的には、コード生成をしたりだとかASTをいじったりみたいなことが好きです。そういう話ができる人や場所を募集してます。twitterなどでmentionなりを飛ばしてくれたりフォローしてくれたりすると嬉しいです。あるいはそのようなslackなりdiscordなりがあれば参加したいです。つまりこの記事の本題は友達募集中という感じのものでした。おしまい。

