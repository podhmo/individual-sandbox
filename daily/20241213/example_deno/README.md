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

## 04 長いURLが死ぬ

こういうamazonの商品詳細のURLなどを送ることができない。たぶんテキスト自体を縮めてfacetsでlinkを渡していたりしていそう。

> Failed to post to Bluesky Error: Invalid app.bsky.feed.post record: Record/text must not be longer than 300 graphemes

```
https://www.amazon.co.jp/Kindle-6%E3%82%A4%E3%83%B3%E3%83%81%E3%83%87%E3%82%A3%E3%82%B9%E3%83%97%E3%83%AC%E3%82%A4-%E9%9B%BB%E5%AD%90%E6%9B%B8%E7%B1%8D%E3%83%AA%E3%83%BC%E3%83%80%E3%83%BC-16GB-%E3%83%96%E3%83%A9%E3%83%83%E3%82%AF-%E5%BA%83%E5%91%8A%E3%81%AA%E3%81%97/dp/B0CP31L73X/ref=mp_s_a_1_1?crid=29GAFXUGABQQ&dib=eyJ2IjoiMSJ9.tAy0qpDNl1JuUuzsizhU_4wzoqcfRXqmCDcVaZMFE9azHwM5LYgVjXr0Adf0fTv_O2RI-z6IetWf3IZYjE8ua4SYMPsG_jr9BNTuekDCIo6YcLfOJMGup20qxtqoK96WZBBiUZuag4QKtNBNzMHiFzeMOtHQz80mgmi_S2p-pqo5zCi63TiRZ26afdKeo1HZC1faon5t6n3tWPsCpOLpAw.vpMLv2crttDL9dziqILs7rcreRc7y0tmHC8gKJ_By2s&dib_tag=se&keywords=kindle+%E3%82%BF%E3%83%96%E3%83%AC%E3%83%83%E3%83%88&qid=1734143731&sprefix=kin%2Caps%2C203&sr=8-1
```

この前者がわからない

- amazonの表示に対応しているか？
- 長いURLに対応しているか？

少し縮めてみる

```
https://www.amazon.co.jp/dp/B0CP31L73X/
```

## 05 facetsを書き換えるいたずらができてしまう

例えばこんな感じにリンク先のサムネイルと表記とは関係ないリンク先に飛ばせられる？

```diff
@@ -124,13 +124,26 @@ async function postToBluesky(
         const urlMatch = content.match(/https?:\/\/\S+/);
         let ogpData = null;
         if (urlMatch) {
-            ogpData = await fetchOGP(urlMatch[0]);
+            ogpData = await fetchOGP(
+                "https://zenn.dev/podhmo/scraps/fd66ac7dd07846",
+            );
         }
 
         // Create RichText with facets
         const richText = new RichText({ text: content });
         await richText.detectFacets(agent);
 
+        // いたずら (リンクをexample.netに変更)
+        if (richText?.facets?.length !== undefined) {
+            for (const facet of richText.facets) {
+                for (const feature of facet.features) {
+                    if (feature.$type === "app.bsky.richtext.facet#link") {
+                        feature.uri = "https://example.net";
+                    }
+                }
+            }
+        }
+
         // Attach OGP data if available
         let embed = undefined;
         if (ogpData && ogpData.ogImage) {
@@ -149,9 +162,10 @@ async function postToBluesky(
             embed = {
                 $type: "app.bsky.embed.external",
                 external: {
-                    uri: ogpData.ogImage,
-                    title: ogpData.ogTitle || "",
-                    description: ogpData.ogDescription || "",
+                    uri: "https://example.net",
+                    title: "クリックしないでください",
+                    description:
+                        "サムネイルはzennのものだしカードのurl表記はexample.netです",
                     thumb: {
                         $type: "blob",
                         mimeType: blobRes.data.blob.mimeType,
```

## 06 fetch を渡せるようだった。

> fetch を持ってこれた
> 
> https://github.com/bluesky-social/atproto/blob/main/packages/api/README.md#bring-your-own-fetch


## 07 型を真面目に付ける

structural subtypingなのでいいでしょということでRefとかその場で定義していた。
あと型のない形でembedとかを記述していたのでハマったときにだるかった(post()の引数でしか制約がかからないので)。

> 型のあり方がわかんなかったdist以下か
> 
> https://www.npmjs.com/package/@atproto/api?activeTab=code
> 
> package.jsonをのぞくとdist/index.d.tsと言ってるし、npmのほうファイルにはdist/index.d.tsが含まれてて便利。雑にぜんぶexportっぽい。それはそうか。
> 
> （ところでnpmの方にはpermalinkが無い？）

package.json

```
  "main": dist/index.js,
  "types": dist/index.d.ts,
```

### deno + .d.ts + `import * as <> from ".dts"` の問題？


何か上手く `import type { ComAtprotoRepoStrongRef } from "npm:@atproto/api@0.13.20";` とかできないみたい。

```console
$ deno check *.ts
Check file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/main.ts
Check file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/ogp.ts
error: TS2709 [ERROR]: Cannot use namespace 'ComAtprotoRepoStrongRef' as a type.
    let root: ComAtprotoRepoStrongRef | undefined = undefined;
              ~~~~~~~~~~~~~~~~~~~~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/main.ts:123:15

TS2709 [ERROR]: Cannot use namespace 'ComAtprotoRepoStrongRef' as a type.
    let parent: ComAtprotoRepoStrongRef | undefined = undefined;
                ~~~~~~~~~~~~~~~~~~~~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/main.ts:124:17

Found 2 errors.
```

ついでにgoto definitionの位置が `~/.cache/deno/npm/registry.npmjs.org/@atproto/api/0.13.20/dist/client/types/com/atproto/repo/strongRef.d.ts` とかになっていた。そしてここには型の定義がない。

```ts
/**
 * GENERATED CODE - DO NOT MODIFY
 */
import { ValidationResult } from '@atproto/lexicon';
export interface Main {
    uri: string;
    cid: string;
    [k: string]: unknown;
}
export declare function isMain(v: unknown): v is Main;
export declare function validateMain(v: unknown): ValidationResult;
//# sourceMappingURL=strongRef.d.ts.map
```

他の箇所でこれをnamespaceとしてimportしていてそれが参照されてしまっているみたいだ。

```
./client/types/app/bsky/feed/like.d.ts:import * as ComAtprotoRepoStrongRef from '../../../com/atproto/repo/strongRef';
```

### 追記:

あ、StrongRef.Mainか。。それはそれで。。import typeができなくない？

こういうのはナンセンスだと思ったりはするのだよな

```ts
import { type Main as ComAtprotoRepoStrongRef } from "npm:@atproto/api@0.13.20/dist/client/types/com/atproto/repo/strongRef.d.ts";
import { type Main as AppBskyEmbedExternal } from "npm:@atproto/api@0.13.20/dist/client/types/app/bsky/embed/external.d.ts";
```

### 追記:

そして型定義が壊れていそう？

```console
$ deno check *.ts
Check file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/main.ts
Check file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/ogp.ts
error: TS2353 [ERROR]: Object literal may only specify known properties, and '$type' does not exist in type 'BlobRef'.
                        $type: "blob",
                        ~~~~~
    at file:///home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241213/example_deno/main.ts:161:25

    The expected type comes from property 'thumb' which is declared here on type 'External'
        thumb?: BlobRef;
        ~~~~~
        at file:///home/po/.cache/deno/npm/registry.npmjs.org/@atproto/api/0.13.20/dist/client/types/app/bsky/embed/external.d.ts:16:5
```

このカードのときの問題が型として再現する。

- https://gist.github.com/podhmo/c9bcef83c88e40b38fb3eb7519b6cc56#03-%E7%94%BB%E5%83%8F%E3%81%A7%E3%81%AF%E3%81%AA%E3%81%8F%E3%82%AB%E3%83%BC%E3%83%89%E3%81%A8%E3%81%97%E3%81%A6%E8%A1%A8%E7%A4%BA%E3%81%95%E3%82%8C%E3%81%A6%E3%81%BB%E3%81%97%E3%81%84

## 08 import typeを諦める

namespaceとしてのimportをそのまま使ってMainを型として使う。

## 09 amazon用のcardを作る

https://jsr.io/@podhmo/ogp を作った。
og:imageが取れない場合にもexternalで埋め込むようにした。