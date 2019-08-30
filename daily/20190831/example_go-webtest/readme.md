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
