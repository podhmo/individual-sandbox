## go examples httptest

https://github.com/podhmo/go-webtest を更新するために色々整理してみることにする。

とりあえずテストコードを空で書く？

- recorderを使ったもの
- serverを使ったもの

そこまではできるとして次に気にするのは何だろう？

- methodを変える
- requestを変える (query string, body)
- responseを見る
- status codeの変遷を見る

このときのテストの描き心地の良さってなんだろう？

- statusの変化があったときにresponseが観える

  - 例えば404の時にそのメッセージがみたい
  - 400のときもか。

- statusの変化があったときにrequestが観える

  - 400のときには実際に送信されたrequestがみたいな

- (snapshotが取られていると嬉しい？)
- integration testとしては db側のデータの整合性チェックもしたい所

もう少し気になる所

- 意図しないrequestが弾かれる
- 意図しないresponseを返さない
- 予期しないエラーのときにはヤバイresponseを返さない
- 予期しないエラーのときには原因を分かるように通知する(e.g. stacktrace)

前者の方は構造の話でschemaで定めてしまいたい所。
型で整合性チェックはできたら理想(go-swaggerとかはglueが多すぎて結局微妙ではあった)。

後者はある意味api server自身の設定が上手くできているかのテストなのでレイヤーが微妙に異なるかも。

付帯事項として簡単なコードはunit testsなしでintegration testsだけで済ませるということがけっこうあるかもということがある。

その他理想的な話をするならなるべくテストの実行時間は短くしたい。テスト用のdbの作成は一度きりになっていると手軽。テスト関数は分けられた方が嬉しい(１つのファイルにt.Run()で頑張るとかは避けたい)。

CIなど回すときに同時に実行したい。これはパッケージ単位で並行に実行できれば十分なのでは？

### 追記

[とりあえずで書いてみた結果](https://github.com/podhmo/individual-sandbox/tree/master/daily/20190731/example_gowebtest/myapi)

### 追記

全体像ははあくした。次は何をしようかな。この辺りを気にする必要があるかもしれない。

> 何をappのコードに書いて何をlibraryのコードに書くか的な話

myapiの部分にmyapitest的な概念を設けてそれを試してみる？テストコードがちょっと変わるかも。
(同一のパッケージでは無理なのでgo.modを生成することになるかも？)

### 追記

個別にNewTestServer,NewTestHandlerを生成するのはあり？なし？

ありといえばあり。

- recorderを使うときのchi.Routerの生成は少なめにしたい気持ちはあったりするかも
- responseに関しては200(OK)をデフォルトにしたい
- handlerのpathへのbindがtestコードと実装コードで二重に定義されるのは嫌かも。

#### handlerの定義

- どこかのタイミングでhandlerのパッケージを分けたくなるかも
- (そのタイミングでBindHandlers()を分割していくというのはありな気がする)
- 特定のhandlerだけを利用してテストみたいなことしたくなることある？
- APIサーバーは共有した方が楽なんだろうか？

### 追記

だいぶどちらで実装するべきか区別が付くようになってきたかも。
httptest.Serverのようなserver側の実装は全部app側の持ち物なきがする。
素直にclient用のwrapperを作ってあげてそれはlibraryになって良さそう。

#### ctxhttp

そう言えばctxhttpってあったな。

https://godoc.org/golang.org/x/net/context/ctxhttp

#### out of context

fooパッケージのXXXAとYYYAをパッケージを分けてxxx/Aとyyy/Bにした時にけっこうめんどくさいことが起きるな。

```
foo
```

循環importを避けるためにfoo/interfacesも必要になる(fooパッケージでnewXXXAとnewYYYAを提供した場合)。

```
foo
foo/xxx
foo/yyy
foo/interfaces
```

### 追記

Adapterを定義してあげたらほとんどresponse生成をwrapするclosureだけの違いになった。

### 追記

あと気になるところはどこだろう？

- postDataなどをfunctional optionsで扱うようにする
- errorの扱いを丁寧にする感じ
- ctxhttpを気にしなくて大丈夫？
- middlewareの追加などを考えなくて大丈夫？

そしてようやく

- snapshot.Take()などとの兼ね合いを考える

にたどり着く

### http

- handler (router)
- persistent
- authentication / authorization
- middleware

  - panic recovery
  - logging
  - 外部APIの利用
  - 外部のmiddlewareの利用
  - sqsとかjob queueの利用

### router

https://github.com/go-chi/chi 辺りを使おう。

- handlerの登録とhandler内の関数が遠のくのは不便かも？

まぁ本題はそこではないので。

#### memo

httptest.Recorderにそのままgo-chiを使ったHandlerFuncを使おうとすると以下の様なエラーが出る。

```
panic: interface conversion: interface {} is nil, not *chi.Context [recovered]
        panic: interface conversion: interface {} is nil, not *chi.Context
```

### その他周辺の情報

- 何をappのコードに書いて何をlibraryのコードに書くか的な話

  - loggerやpanic recoveryのテスト
  - confからのcomponent(component factory)の生成
  - DI的な話

- 提供するデータの形の変更

  - graphQL対応
  - (後から) api documentへの対応 (e.g. openapi doc)
  - 古い(負債になった) api documentのフレームワークの廃止

- 実行形態の変更

  - FaaS対応
  - CLI対応
  - grpc/grpc-web 的な話

- openTracing的な話
- monitoring的な話
- deploy (provisioning, orchestration) 的な話
