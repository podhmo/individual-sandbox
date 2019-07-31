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

### http

- handler (router)
- middleware
- persistent
- authentication / authorization

### router

https//github.com/go-chi/chi 辺りを使おう。
