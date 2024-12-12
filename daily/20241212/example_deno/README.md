# bsky

## 00 password input

パスワードとidentifierを入力して表示する

## 01 login bluesky

入力したpasswordなどを使ってログインする
📝 passwordはmaskしたいかも？(defaultでmaskする？)

テキトーに.envに保存することにした.configファイルがほしいかも？

## 02 auth subcommandを作る

そういえばサブコマンドを作る例を書いていたな。
https://github.com/podhmo/with-help/blob/main/examples/build-subcommands.ts

サブコマンドを書くのはだるいね。。

```ts
interface BaseOptions {
    debug: boolean;
}

async function main() {
    const baseOptions = parseArgs(Deno.args, {
        name: "bsky",
        boolean: ["debug"],
        stopEarly: true, // for subcommand
        footer: `
Available Commands:
  auth: authentication for bluesky`,
    });

    const args = baseOptions._;
    if (args.length === 0) {
        console.error("%cneed command", "color: red; font-weight: bold");
        printHelp(baseOptions);
        return;
    }
    switch (args[0]) {
        case "auth":
            await authCommand(args.slice(1), baseOptions);
            break;
        default:
            console.error(
                `%cunknown command: ${args[0]}`,
                "color: red; font-weight: bold",
            );
            printHelp(baseOptions);
            break;
    }
}
```

## 03 auth statusを作る

auth statusと言いつつauth doctorのようなものかも？

- https://docs.bsky.app/docs/api/app-bsky-actor-get-profile

access tokenからdidかhandleを手に入れる方法はあるんだろうか？
:thought-balloon: .envではなくconfigに情報を書き込みたいかも？
baseurlを表示したらまずいのでは？

```console
$ deno run -A main.ts auth status
https://bsky.social/xrpc
  ✅️ Logged in to https://bsky.social/xrpc
  handle: podhmo.bsky.social
  display name: podhmo
  access token: **********... length=337
```  

## 04 auth tokenを作る

auth tokenを作るときに`--access-token`をrequiredにしておくのは何か変な気がする？

## 05 auth refreshを作る

基本的にはrefresh tokenで認証するだけ。
この辺書いていてどのfetchを呼ぶ実装なのかわからなくなってきた。

❇️ denoのdotenvはコメントに対応しているみたいなので便利だった

## 06 投稿する処理を書く

雑に書いてみる。
こちらはmainをそのまま渡せれば良い気がする。

```console
$ deno run -A main.ts post hello world
post success{
  uri: "at://did:plc:hpog7qvhzybjzzjq3p5eq6ei/app.bsky.feed.post/3ld4gd4lytr2i",
  cid: "bafyreiccmxud4vop4i4l6arf6rja25tktos5566zdzp5zetaqguqpnarza",
  commit: {
    cid: "bafyreidnbhaa2vek3zh4sgcbzvsmejavvgzesa23woowmw2wvst5yjjf5q",
    rev: "3ld4gd4m7oj2i"
  },
  validationStatus: "valid"
}
```

このあとスレッドを追加したりmentionを追加したりしないとダメそう。

## 07 とりあえず引数を分けてスレッドで保存する処理を書く

この辺awaitのためにfor-ofを使うことになるけれど末端だけ返すみたいなコードを書くのがだるいな。。

```console
# hello -> world というスレッドになる
$ deno run -A main.ts post hello world
```

## 08 URLのリンクを反映させる

facetsというものを設定しないとだめなのか

https://docs.bsky.app/docs/advanced-guides/post-richtext
https://www.npmjs.com/package/@atproto/api#rich-text

```diff
--- a/daily/20241212/example_deno/main.ts
+++ b/daily/20241212/example_deno/main.ts
@@ -2,6 +2,7 @@ import "jsr:@std/dotenv/load";
 import { parseArgs, printHelp } from "jsr:@podhmo/with-help@0.5.2";
 import { promptSecret } from "jsr:@std/cli@1.0.8/prompt-secret";
 import { withTrace } from "jsr:@podhmo/build-fetch@0.1.0";
+import { RichText } from "npm:@atproto/api@0.13.20";
 
 
@@ -469,6 +469,12 @@ export namespace Bluesky {
         for (const content of input.contents) {
             const createdAt = input.createdAt ?? new Date().toISOString(); // 迴ｾ蝨ｨ譎ょ綾繧棚SO 8601蠖｢蠑上〒蜿門ｾ・ 
+            // for: mentions and links https://docs.bsky.app/docs/advanced-guides/posts#mentions-and-links
+            // https://www.npmjs.com/package/@atproto/api#rich-text
+            const rt = new RichText({ text: content });
+            const agent = undefined; // TODO: agent
+            await rt.detectFacets(agent); // detect facets such as links and mentions
+
             const res = await fetch(
                 `${BASE_URL}/com.atproto.repo.createRecord`,
                 {
@@ -481,7 +487,8 @@ export namespace Bluesky {
                         collection: "app.bsky.feed.post",
                         record: { // https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/post.json#L9
                             $type: "app.bsky.feed.post",
-                            text: content,
+                            text: rt.text,
+                            facets: rt.facets,
                             createdAt,
                             // langs: ["ja", "en"], // TODO: langs
                             reply: root ? { root, parent } : undefined, // https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/strongRef.json

```

ただこれをやってもリンクとして機能するだけでPWAなどがやっているthumbnailを付与したカード的な見た目のリンクにはならないみたい。

# references

- https://jsr.io/@std/cli/doc/prompt-secret/~/promptSecret
- [gist -- bskyにスレッドで投稿したい](https://gist.github.com/podhmo/19a4e189dd5c9a8d3af871139c51b9fe)
- https://docs.bsky.app/docs/api/app-bsky-actor-get-profile
- https://docs.bsky.app/docs/advanced-guides/posts
- https://docs.bsky.app/docs/advanced-guides/post-richtext