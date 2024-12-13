# bsky clientの続き

昨日の続き

- https://gist.github.com/podhmo/ca18aa3f456dd1aba25faba2200eda44

サブコマンドにして機能を盛り盛りにしてしまった反省があった。
あとdidやhandleは後で回収できるかも？
そしてreply対応に結局atprotoが必要そうなので一度そのまま依存して作ってみることにしてみる。

## 00 chatgptに生成してもらう

以下の様なプロンプトで生成してもらったコード。 https://chatgpt.com/share/675c4296-1d84-8001-a1e0-7acba5f414bd

```
このコードは以下を実現します：

1. **ログインとトークンのキャッシュ**：ログイン後に`did`、`accessJwt`、`refreshJwt`をキャッシュします。
2. **アクセストークンの自動更新**：トークンが期限切れの場合に`refreshJwt`を使用して更新します。
3. **OGPデータの取得**：リンクが検出された場合にOGPデータを取得します。
4. **リッチテキスト投稿**：`RichText`を使用して投稿にfacetsを添付します。
5. **スレッド投稿**：配列で渡された内容を順番にスレッドとして投稿します。

必要に応じて、このコードをカスタマイズしてください。
```

ちなみのこのプロンプト自体もChatGPTとの会話によって手に入れたもの

- https://chatgpt.com/share/675b77d8-89bc-8001-85c5-e2e3264806f2

## 01 denoで動くように変更してみる

- npmパッケージを使うようにimportを変更
- DOMParserを利用できるように修正 https://github.com/b-fuze/deno-dom
- refresh tokenの実装が読み込めないみたいなのでコメントアウト
- parseArgs()を追加
- dotenvの読み込みを追加
- 型をあわせる

思ったのはライブラリを使うと不要なメソッドの補完候補が多いかも？あとimportする型を探しに行くのがめんどくさい(雑に手元で定義した)
fetchを差し替える方法とか調べるのがめんどくさいな。。。

以下の様なエラーが出てだるい。多分不足しているのだろう。

> Failed to post to Bluesky Error: Invalid app.bsky.feed.post record: Record/embed/images/0 must have the property "image"

## 02 埋め込みのコードが全然おかしいことに気づく

- https://github.com/bluesky-social/atproto/blob/13636ba963225407f63c20253b983a92dcfe1bfa/lexicons/app/bsky/embed/record.json#L54
- https://github.com/bluesky-social/atproto/blob/13636ba963225407f63c20253b983a92dcfe1bfa/lexicons/app/bsky/embed/images.json#L36

何か型の構造が変わってるじゃん。。blobにploadする必要があった（使い回せないんだろうか？）。

- https://docs.bsky.app/docs/advanced-guides/posts
- https://docs.bsky.app/docs/api/com-atproto-repo-upload-blob