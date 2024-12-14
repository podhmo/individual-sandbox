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

## 03 画像ではなくカードとして表示されてほしい

"app.bsky.embed.images" ではなく "app.bsky.embed.external" を利用する必要があるみたい。

https://docs.bsky.app/docs/advanced-guides/posts#website-card-embeds

素直にドキュメントのまま利用するとエラーになってしまう。

> Failed to post to Bluesky Error: Invalid app.bsky.feed.post record: Record/embed/external/thumb should be a blob ref

この辺の型を持ってくる手軽な方法は何なんだろう？

```ts
export declare class BlobRef {
    ref: CID;
    mimeType: string;
    size: number;
    original: JsonBlobRef;
    constructor(ref: CID, mimeType: string, size: number, original?: JsonBlobRef);
    static asBlobRef(obj: unknown): BlobRef | null;
    static fromJsonRef(json: JsonBlobRef): BlobRef;
    ipld(): TypedJsonBlobRef;
    toJSON(): unknown;
}
```

これ見ると渡せるのはblobだけ？

https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/embed/external.json

devtoolでwebuiを覗いて見て通信を見てみるとこんな感じ？

```json
{
    "$type": "app.bsky.feed.post",
    "createdAt": "2024-12-13T18:39:03.950Z",
    "embed": {
        "$type": "app.bsky.embed.external",
        "external": {
            "description": "Bluesky posts are repository records with the Lexicon type app.bsky.feed.post.",
            "thumb": {
                "$type": "blob",
                "ref": {
                    "$link": "bafkreich56ppsya5nphhjbto5q2td4yww7ypjiwioa2yat3koniqhijl7u"
                },
                "mimeType": "image/jpeg",
                "size": 46954
            },
            "title": "Creating a post | Bluesky",
            "uri": "https://docs.bsky.app/docs/tutorials/creating-a-post#website-card-embeds"
        }
    },
    "langs": [
        "ja"
    ],
    "text": "externalの指定が正しかったようだ\n\ndocs.bsky.app/docs/tutoria..."
}
```

通った。。何なんだ。。