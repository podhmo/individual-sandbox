# go name error

goは型名と値名を持つ場所は同じらしく同一スコープで同一名の型定義をするとname conflictエラーになる。

- [example_inner_struct/](./example_inner_struct/)

# facebook oauth facebook認証

- [昨日の続き](../20161025)

前提条件

- 事前にfacebook認証用のapiのセットは用意されている
- app側の認証はAuthorizationヘッダーを見る感じ(jwt)

色々と条件が厳しくcurlやhttpieでシミュレートするのは辛い。
結局フルのブラウザを使うのが楽。

辛いところ

- ログイン処理にjsが使われている
- ログイン時のrefererなどを見ている感じがする？
- ダミーアカウント的な雑な入力で作ったアカウントは1日経つと本人確認を要求される

結局やったこと

- facebookにapp登録
- ngrokでlocalhostのサービスを公開する
- ngrokの公開URLをコールバックURLに設定する

app側の条件に対応するためにやったこと

- proxy2.pyをいじって、常にAuthorizationヘッダーを付加するようにする
- 必要なURLを組み立ててappサーバーにリダイレクトするwsgiアプリを作る。

```
wsgi appにアクセス
- redirect -> app server
- oauth redirect -> facebookでログイン処理
- appのcallback redirect -> yay!
```

