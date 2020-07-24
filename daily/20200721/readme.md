## まじめにbotを考えてみる

slack botやdiscord bot?

- デプロイというほど管理したくない

  - 設定情報もtokenも管理したくない
  - 各ユーザーの設定情報も基本的にはユーザーしか閲覧できないようにしたい
  - 可能ならdbを持ちたくない
  - 可能なら開発中はローカルだけで動かしたい


- reactionに反応して何らかの操作を行いたい

  - 例えば、特定の発言にreactionをつけると特定のrepositoryにissueを作成
  - (このときissue templateを気にしたい(?))
  - reactionをSQSのmessageに変換する機能があっても良い (ここだけ浮いている)

- 各操作をplugin的な形で提供したい

  - eager-loadとlazy-loadが欲しい
  - pluginの読み込みだけを行って止まるモードが欲しい (validation)

- aliasでのechoが欲しい

  - この種のものは各シート(名前空間)が分かれていると困る (KVSの項)
  - slackのid(name)とaliasを紐づけたい

- アカウントのoauth連携をbot上でやりたい

  - (slackのid(name)とgoogle accountを紐づけたい)
  - (slackのid(name)とgithub accountを紐づけたい) (会話の項)
  - tokenの再生成も同様の手順で行いたい
  - しっかりと不要になったtokenはrevokeしたい

- APIの実行中などの情報をchat service上に載せたい

  - 実行予約・実行中・完了・中断・エラー
  - 何らかの設定をつけると、エラーレスポンスなどが確認可能な状態にしたい
  - 呼び出し回数などを記録したい (log出力? event emit?)

- API callに対してrate-limitやthrottleを設けたい

  - domain毎の同時リクエスト数を制限
  - (もちろん非同期)
  - 荒ぶっているユーザーとAPIリクエストを監視可能にしたい

- 各自が自分のKVSを持ちたい

  - spreadsheetを共有させるのが良いのでは？ (db持ちたくない。連携をユーザーが切れる)
  - いい感じに共有してURLを渡すUIが欲しい (会話の項)
  - (他の方法で代替できても良い)
  - syncが遅いので差分があったらsyncを非同期で動かす必要があるかも？

- botと標準入出力での会話であるかのように対話したい
