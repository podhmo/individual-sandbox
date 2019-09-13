# go go-webtest

https://github.com/podhmo/go-webtest

何が必要かも含めて体験を整理したほうが良いかもしれない。

- tripperwareの実装は良かった。これによってhookとroundTripperDecoratorを一緒にできた
- オプションがちょっと冗長な気がする
- http.Responseのcopyを使うことにした。testclient.Responseの存在領域が減った
- ところどころメソッド名などを整理した

この辺りがハイライト。
あと気にする部分はなんだろうか？

## 追記

全体をつなげてみる？
hello world的なhandlerばかりをやっているような気がする？
ポイントとしては完成形以外のテストの書き味的なものを意識したいのだよなー

1. リクエスト先が間違って該当のhandlerに届いてない
2. 該当のhandlerへのリクエスト先は間違っていないものの途中でエラー扱いになった
3. 該当のhandlerには届いたが途中で意図しないエラーを返した
4. 該当のhandlerに届き正常なレスポンスを返したがそれは意図しないstatusのものだった

## 追記

まだ記述量が多い気がしている。あと複数のパッケージを使い分ける必要があるのがめんどう。
あとjsonのdiffの表示がまだあまり良くないかもしれない。

## 追記 とりあえず１から順にコードを書いてみる

本当にシンプルなhandlerとテストを書いてみる。
環境変数経由でのオプション指定はmakefile経由での実行でも機能する点が便利かもしれない。

```console
$ make 04
cd 04* && go test
{"result":6}

PASS
ok  	m/04add	0.006s
```

debug trace付き

```console
$ make 04 DEBUG=1
cd 04* && go test
2019/09/07 16:27:47 builtin debug trace is activated
	Request : ------------------------------
	POST / HTTP/1.1
	Host: example.com
	Content-Type: application/json
	
	{"values": [1,2,3]}
	----------------------------------------
	Response: ------------------------------
	HTTP/1.1 200 OK
	Connection: close
	Content-Type: application/json
	
	{"result":6}
	----------------------------------------
{"result":6}

PASS
ok  	m/04add	0.003s
```

このdebug traceができるのが該当するhandlerに到達できなかったかどうか調べるのに便利。
(適切なhandlerに到達できたかどうかの逆側のデバッグ情報は、幾つかのフレームワークでDEBUGオプションを付けた時に手にはいる(e.g. go-swagger)。一方でこのときのrequest情報は手に入らなかった)

初手の使用感としては悪くない。

認証をテストコードが突破できるかというタスクが間にはいることもあるかもしれない。

### 追記

正しいhandlerに到達できたら、次はhandlerの実装部分の不備を直すということになる。実務のコードでは「なぞのvalidation errorで失敗。おしまい」みたいな表示になるテストがけっこうあったりする。そしてそのような貧弱な出力のテストの修正にけっこうストレスが溜まる。

そのようなテストの失敗も意外とhandlerの不備ではなくテストコードで渡すrequestの不備であることも多い。結局そういうテストの失敗を直すにはAPIのドキュメントを見て実装コードを見て正しいinputが何かを調べて、ということになりがち。消耗する。

例えばvalidation errorで失敗のときなどにはそのresponseまでを含めて見たい（400 BadRequestだけでは情報量が少ない）。このresponseを含んだエラーメッセージは、assertionの書き方で対応できるしするべきだけれど、それが行われていない場所でも最悪traceして動かせばどうにかなるというのはうれしい(ちょっとだけコードを書き換えてprintデバッグした経験無い？個人的にはけっこうある)。

```console
$ DEBUG=1 make 04
cd 04* && go test
2019/09/07 16:43:18 builtin debug trace is activated
        Request : ------------------------------
        POST / HTTP/1.1
        Host: example.com
        Content-Type: application/json

        {"values": [1,2,"foo"]}
        ----------------------------------------
        Response: ------------------------------
        HTTP/1.1 400 Bad Request
        Connection: close

        {"error": "json: cannot unmarshal string into Go struct field Input.Values of type int"}
        ----------------------------------------
```

ただ環境変数に頼っての結果の確認というのは、２度目の実行が必要になるということなので、assertionもまともにしたほうが良い。そうでなければCIで常に冗長な出力(DEBUG=1で実行)で妥協か手元で失敗したテストを再実行することを強制されることにになってしまう。

意外とCIの画面上で長々と出力されたエラーメッセージを追うのは苦痛だったりする（これはCI環境のコンソールの出力画面が長い出力をみるにはストレスフルな状態であることが多いということにも起因するのかもしれない。後その場でgrepとかできない）。

### 追記

というわけで、テストではまじめにassertionを書く。responseをまじめに確認する前にエラーの有無やステータスコードを確認するというのは自然な行為ではあるのだけれど。ここでしっかりとテストが失敗した時にresponseのbodyを出力してあげるようにするのが良い。

例えば今回は200のつもりだったが400を返している例。

```console
$ make 04
cd 04* && go test
--- FAIL: TestHandler (0.00s)
    add_test.go:19: Equal, expected 200, but actual 400
        body is {"error": "json: cannot unmarshal string into Go struct field Input.Values of type int"}
FAIL
exit status 1
FAIL    m/04add 0.002s
```

何度も繰り返しになってしまっているけれど、ここで `expected 200 but got 400` とだけ出てくるのはけっこうツライ。幾つかある不正な入力の可能性から誤りであったrequestのパラメーターなどを特定しなければいけない。めんどくさい。特に途中参加したプロジェクトなどでテストの実行に時間がかかっている上でそのような状況だとストレスがやばい（やばい）。

せめて発生したエラーなども出てきて欲しい(`body is {"error": "json: cannot unmarshal string into Go struct field Input.Values of type int"}` とか。あるいは利用したvalidationライブラリのvalidation erroroとか)。

まともなフレームワークを使っていたりまともな環境でコードを書いていれば何らかのエラー原因がresponse bodyに含まれるはずなのでそれをテストコードで潰してしまうのは。。。良くないことですね。。。

#### 寄り道

以下の様なこともふわっと思ったりしますがとりあえずは置いておきましょう

- 同様に 500, InternalServerError だけというのも情報量ゼロ

  - (ただし本番環境ではその様になっていて欲しい）
  - (もちろん裏側ではログの収集とエラー通知は入っていますね)

- domain error, application error を error responseにマッピングする行為のだるさ

  - (`type MyHandlerFunc func (http.ResponseWriter, *http.Request) error` として `ServeHTTP()` を実装するのもありと言えばありだが。。)
  - (普通はエラーハンドリングのためのミドルウェアやpanicn抑制のためのミドルウェアが入っているはず)
  - (https://github.com/srvc/fail とか使ってはじめから情報量豊かなエラーにしておくのもありな気がする)

### 追記

正しいinputのように変えたところで先に進める。200 OK的なresponseが返ってくることここまででようやくスタートラインという感じ。一方で依存が多かったり実装がベタ書きだったりするとここまでが長い。

responseとrequestの形式をJSONでという形の方が柔軟な一方で、内部で利用している型定義が使えたらすこしだけ頑健になるという話もあったりする。一方でそれが利用するフレームワークに密結合だった場合などには捨てづらくなるというような問題も起きる。

ゆるふわはゆるふわでフィールド名のミスなどでエラーにしがちかもしれない(あるいは比較の方法によっては型の違いによるエラーがだるいと言うようなこともあるかもしれない)。

```console
$ make 04
cd 04* && go test
--- FAIL: TestHandler (0.00s)
    add_test.go:25: unexpected error, not equal json
        left:
                {"result":6}
        right:
                {"n":6}
        on equal check
FAIL
exit status 1
FAIL    m/04add 0.033s
make: *** [Makefile:6: 04] Error 1
```

### 追記

そろそろ出来上がったコードを見てみる。

正直１つのAPIのテストコードにして長い気がする？

あとstatusチェックの部分での `"body is ", got.LazyText(),` が自由記述領域感があってあんまり増やしたくない（気がする）。

```go
func TestHandler(t *testing.T) {
	c := webtest.NewClientFromHandler(http.HandlerFunc(Add))
	got, err := c.Post("/",
		webtest.WithJSON(bytes.NewBufferString(`{"values": [1,2,3]}`)),
	)

	noerror.Must(t, err)
	noerror.Should(t,
		noerror.Equal(200).Actual(got.Code()),
		"body is ", got.LazyText(),
	)

	noerror.Should(t,
		jsonequal.ShouldBeSame(
			jsonequal.From(got.JSONData()),
			jsonequal.From(map[string]int{"n": 6}),
		),
	)
}
```

tripperwareを使ってみる。これはRountTripperのmiddleware的な表現という意味のもの(後で詳しく説明するかもしれない。しないかもしれない)。

```go
	got, err := c.Post("/",
		webtest.WithJSON(bytes.NewBufferString(`{"values": [1,2, "foo"]}`)),
		webtest.WithTripperware(
			tripperware.ExpectCode(t, 200),
		),
	)
```

結果はこうなる。

```console
$ make 04
cd 04* && go test
--- FAIL: TestHandler (0.00s)
    add_test.go:23: unexpected error, status code, expected 200, but actual 400
         response: {"error": "json: cannot unmarshal string into Go struct field Input.Values of type int"}
FAIL
exit status 1
FAIL    m/04add 0.003s
make: *** [Makefile:6: 04] Error 1
```

responseがきれいではないかもとか色々あるかもだけれど。とりあえず分かる状態にはなっている。

### 追記

結果の確認もしたい。テストの一部はリグレッションテスト。リグレッションテストの一部は前後比較。以前動いていたものと同じになるなら正しいということにしてあげれば良いのでは？というのがスナップショットテスト。

以前のrequestとresponseを保存しておいて比較してあげればスナップショットテストが実現できそう。goでは正解データ(goldenファイル)をtestdataに置くという慣習があるのでそこに以前の結果を保存してあげることにする。

```go
func TestHandler(t *testing.T) {
	c := webtest.NewClientFromHandler(http.HandlerFunc(Add))
	var want interface{}
	got, err := c.Post("/",
		webtest.WithJSON(bytes.NewBufferString(`{"values": [1,2,3]}`)),
		webtest.WithTripperware(
			tripperware.ExpectCode(t, 200),
			tripperware.GetExpectedDataFromSnapshot(t, &want),
		),
	)
	noerror.Must(t, err)
	noerror.Should(t,
		jsonequal.ShouldBeSame(
			jsonequal.From(got.JSONData()),
			jsonequal.From(want),
		),
	)
}
```

初回は必ず成功する。以降、変更するには `SNAPSHOT=1` か `SNAPSHOT=<filename>.golden` という形で保存することを強制する(もう少しゆるく指定できたり保存される対象が分かるようにできたほうが良いかもしれない)。

たとえば失敗したときには以下の様な出力になる。

```console
$ make 04
cd 04* && go test
--- FAIL: TestHandler (0.00s)
    snapshot.go:56: load testdata: "testdata/TestHandler.golden"
    add_test.go:25: unexpected error, not equal json
        left:
                {"result":7}
        right:
                {"result":6}
        on equal check
FAIL
exit status 1
FAIL    m/04add 0.003s
make: *** [Makefile:6: 04] Error 1
```

ああ、一応Closeもしておこう。

## 追記

結局、tryパッケージを書き直した。正確に言うと、昔作ったけれど削除したパッケージを復活させた。

```go
func TestHandler(t *testing.T) {
	c := webtest.NewClientFromHandler(http.HandlerFunc(Add))
	var right interface{}
	try.It{
		Code: 200,
		Want: &right,
	}.With(t, c,
		"POST", "/",
		webtest.WithJSON(bytes.NewBufferString(`{"values": [1,2,3]}`)),
	)
}
```

失敗の例は変わらず。

statusがおかしい

```console
$ make 04
cd 04* && go test
--- FAIL: TestHandler (0.00s)
    snapshot.go:56: load testdata: "testdata/TestHandler.golden"
    add_test.go:18: unexpected error, status code, expected 201, but actual 200
         response: {"result":6}

FAIL
exit status 1
FAIL    m/04add 0.003s
```

responseが合わない。

```console
$ make 04
cd 04* && go test
--- FAIL: TestHandler (0.00s)
    snapshot.go:56: load testdata: "testdata/TestHandler.golden"
    add_test.go:18: unexpected error, not equal json
        left:
                {"result":7}
        right:
                {"result":6}
        on equal check
FAIL
exit status 1
FAIL    m/04add 0.003s
```

このときのrequest/response (testdata/TestHandler.golden)

```json
{
  "modifiedAt": "2019-09-07T18:22:31.069934432+09:00",
  "data": {
    "request": {
      "body": {
        "values": [
          1,
          2,
          3
        ]
      },
      "method": "POST",
      "path": "/"
    },
    "response": {
      "data": {
        "result": 6
      },
      "statusCode": 200
    }
  }
}
```

### 追記

💭 `try.It{}.With()` が良い名前なのかは怪しい。

## 追記

jsonequalの表示をもう少し豊かにしても良いかも知れない。
gojsondiffなどを使ってみる？

## 追記

いろいろ調整した。結局gojsondiffではなくこちらを使った。

- https://github.com/nsf/jsondiff

## 追記

エラーメッセージなどを調和する感じにしてみた。
diff部分だけleft-rightの意味合いが強いのだけれど、Eitherぽい感覚でright(want)というふうにしてみた。

## 追記

自分で忘れないようにreadmeにコード例を書いた

- webtestだけを使ったもの
- tryを使ったもの
- webtestを使わないけどsnapshot testはするもの

## 追記

statusチェックの部分のエラーメッセージをdebug traceのものに揃えた。
(debug traceの方はstderrに出るのだよなー。そう言えば)

