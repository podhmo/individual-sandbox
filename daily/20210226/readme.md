## go mongolike

- csvのような何かのqueryに使うのはどうだろう？

  - SQLの代わりとして
  - 行ごとにchannelで出力するとキレイなのではないか？
  - すべてが文字列なものをどうやっていい感じ型をつけるかが悩ましい？

    - `--guess`的なオプションを作れる？
    - `--int64=xxx,yy,zz --float64=aaa --bool=status` 的な形で指定する？
    - `--struct=foo.go` (これはgo/typesなどを使えないとダメなのでは？)

  - https://github.com/dinedal/textql
  - https://github.com/rchowe/textsql/blob/master/textsql

    - sqliteに突っ込むアプローチ

- 実はoutputに標準出力、ファイル出力、s3へのuploadなんかを対応できると嬉しいのでは？

  - 依存が膨れ上がるのが本当に納得できない
  - foo-genのようなコマンドを作成する。fooコマンドを生成するコマンド
  - リポジトリにあるcmd自体はフルセットを生成したものにする。go.modで管理する？
  - 結局依存がめんどくさいのか。。辞めよう。

- いい感じにcollectionをweb APIとして配信したい

  - あるresourceに対して

    - paginationが欲しくなる
    - filteringが欲しくなる (find,where)
    - 特定のフィールドだけを返すものが欲しくなる (project,select)
    - 件数だけが欲しい場合がある (count)
    - どのようなフィールドが存在するのか指定するものが欲しくなる (_schema)
    - (結局、全部mapで考えたほうが楽なんだろうか？(reflect?))
    - (更新系の処理は提供しない？)
    - query実行時に何らかのモニタリングを挟みたくなる
    - (集計処理を加えたくなる (aggregate))
    - (複数のresourceを混ぜ合わせて使いたくなる (join, relationship))

  - API全体に対して

    - どのようなresourceが存在するか知りたくなる
    - health checkの応答を返すものが欲しくなる
    - (アクセス制限をしたくなる事がある (oauth-proxyなどに任せてしまえば？))
    - (流量制限をしたくなることがある (GCPのapigeeとかAWSのapi gatewayとかに任せてしまえば？))

  - responseに対して

    - responseの内容を整形して表示したくなることがある
    - responseとして手元に返って来なくて良いので、非同期で結果をどこかに渡して欲しくなる事がある

  - そもそもwebAPIとして配信する意義はなんだろう？

    - アクセス権を持たない人々に、制限された形でアクセスできるようになる
    - requestの流量や頻度を管理できる
    - 複数のdatabaseの存在を悟られずに１つのものとして扱える (複数DBを１つに)
    - 個々の内部的な実装を隠蔽してくれる (利用するdbや実装の中身を隠蔽)
    - (どのようなデータを返すかのガイドラインを提供してくれる)

  - いわゆるRPCではダメなんだろうか？

    - 上手く作れば行けるのでは？
    - webAPIを作るのは意外とめんどくさくない？みたいなところから端を発している(?)

  - bigqueryではダメなんだろうか？

    - 少量のデータのときにレイテンシーがあまり良くない
    - それこそデータ量がおおくなればこちらのほうが便利なのかもしれない

- graphQLと何が違うんだろうか？

  - graphQLは適用の形態を制限している。それはそれで真っ当で正しい
  - 一方でqueryの条件に自由がない。ANDとORを重ねられない。
  - (graphiQL越しに扱うと便利ではある)


## reflect

- https://www.linkedin.com/pulse/go-performance-tips-iv%C3%A1n-corrales-solera
