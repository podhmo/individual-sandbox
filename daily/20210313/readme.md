## go fullのAPIを考えてみる

- ui

  - openapi doc
  - pagination
  - request validation

- develop

  - logging
  - stack trace on develop env
  - test database
  - gentle error message when test is failed

- handling API request

  - authentication
  - format check
  - authorization (e.g. RBAC)
  - data validation
  - db lookup
  - db update
  - communicate to external services

- functions (?)

  - middleware
  - session
  - (scaffold)
  - db migration
  - monitoring (with extra arguments)

手軽なものを考えると、organizatinとuserのような仕組みの登録を考えるのが良いか。

- /tenant/organicatioin/user
- 別軸でteamが存在する
- userはaccountと紐付き security的なものはそちらに
- userはprofileと紐付き view的なものはそちらに

- 何か外部にメッセージを送信するような操作が欲しい
- 何か特定のタイミングで実行されるようなバッチが欲しい

  - 何かの最新に追いつくというような情報があれば良いのかもしれない
  - incident対応管理みたいなものは考えられる

## せっかくなので真面目に実用的な何かを例にすることにするか

- githubのnotificationをいい感じに取得する君
- RBAC的なpermission管理 (awsのiam的な, PARK?)
- 何かの情報をscrapingないし何かをしてイベントを通知
  - イベントに対する対応事項などを決める
- 会議と議事録を追う何かがあっても良いのではないか？
  - いっその事、databaseをgithubにする
  - これはこれとしてありかもしれない
- flutterで扱う事を考えてみる？
  - なぜgoを使うのかという気持ちになってきた。。

## いい感じの開発体験を提供したい

- いい感じの開発体験を提供するにはどうすれば良いか？

  - 古臭い記述は消していかないとまずい

     - コードと言うよりはツール部分
     - vendoring (glide -> dep -> go modules)
     - embeding (gobindata -> embed)

  - 特定の操作に時間が掛かるようになった場合の対処

     - test
     - lint
     - コード生成

  - それはそれとしていろんな操作が手軽に呼べる様になっていて欲しい
  
     - 明示的であることも嬉しさの一つ
     - 対象を明示的に指定すると選択するの間の違い（補完）

## 裏側の処理を考える

裏側の処理はどのようなコードが良いだろうか？

- 先程まで言っていたrole baseのコミュニケーションのもの
- ちょっとしたtodo

せっかくだから前者にしちゃうか。filteringなどは全部SQLに任せよう。ここでsqlxを使うことになる。

## tenukiを使ってapi serverを作ってみる

1. とりあえずserverを作って繋げてみる
1. 裏側の処理を作る
