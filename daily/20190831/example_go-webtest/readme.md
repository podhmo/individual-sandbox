# go-webtest 何か新しい機能の追加と調整をやるか

何をしようかな。今はどこまでできていたんだっけ？

- ../../20190731/readme.md

- 認証周りを含んだテスト
- headerを追加したテスト
- post dataを追加したテスト
- query stringを追加したテスト
- (proxyのテスト)
- サーバー側のcomponentsの切り替えのテスト

## 追記

それとは関係なく以下のことって手軽にできないかな。

- JSON responseの生成

echoとかにはありそうだけれど。go-chiにはあるんだろうか？

https://godoc.org/github.com/go-chi/chi

あるはずないか。router libraryだし。

- https://godoc.org/github.com/labstack/echo
- https://github.com/labstack/echo/blob/master/context.go

たしかに。query paramterでresponseのpretty printか変えたいのでその情報は見えた方が良いのだよなー。

go-chiでquery parameterを取る方法はどうやるんだっけ？というか通常のnet/httpで。

## 追記

そう言えばclientのinterfaceを切り替えたりしたかったような

## 追記 go-chiの復習

- pathパラメーターを取る

  - chi.RouterContext(req.Context()).URLParam("xxx")
  - chi.URLParamFromCtx(req.Countext(), "xxx")

- querystring

  - chiはrouterライブラリなので範囲外
  - req.URL().Query() を mapとして扱うか Get

- header

  - chiはrouterライブラリなので範囲外
  - req.Header を mapとして扱うか Get

- post form data

  - chiはrouterライブラリなので範囲外
  - req.PostForm を mapとして扱うか Get
  - (req.FormはURL fieldを含む)

- post application json

  - chiはrouterライブラリなので範囲外
  - req.Bodyをparse
  - (まじめにやるならmethodをしらべて,content-typeを調べて)

## 追記

ようやくechoのhandlerをwebtestで使えそう。
とりあえずrecorderを使う方からと思ったけれど。HandlerFuncを期待していて微妙かも？

## 追記

clientのなまえを変えたい

## 追記

通信をtraceしたい。
httptestのrequestを透過的に使いたい
環境変数でtraceを調整したい。

### 以下の操作ができて欲しい

- header付加
- JSONをpost

### だるいこと

- teardownがだるい
- componentsの切り替えはどうすれば良いんだろう？
- やっぱりDoはだるいのでは？
- contextにwrapする方法が無い
- querystringを手軽に追加できない？
- MustDo()とか使いたくない

## やりたいこと

- ok trace的な何か
- testの比較対象をコード側に書きたい(?)
- table driven test
- ok exというパッケージ名前は止めたい
- ok ex.SnapshotTest()という名前は止めたい
- noerror(?) jsonequalの表示を良い感じに

  - 少なくとも順序は一致させたい
  - https://github.com/gavv/httpexpect
  - jsonequalでは？

- 記述だるくない？

  - ok GET(),POST()...とか作る

## 追記

とりあえずtraceをやるか。
stderrでhttptestが見れれば。とりあえず。
RoundTripperが使えるのはTestServerのときだけか。。

## 追記

decoratorを作った。内部的なConfigが欲しくなったし。NewRequestなどもこれを使うのが正解かもしれない。

## 追記

query stringに対応していなかった

## 追記

考えてみればURLを指定してclientを生成しているだけなのか。

## 追記

- WithQuery()に対して MustParseQuery()
- WithJSON()に対して MustJSON()


