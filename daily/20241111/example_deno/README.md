# Youtubeの再生リストを動的に生成したい
## とりあえず、youtubeのチャネルを一覧で取得したい

fetch.ts
その前にデータを取得しておきたい。別にライブラリとかは要らない気がする。 

### API KEYが存在しない

作らないとだめか

### 型の情報がほしい

`npm:@types/youtube` はなんか違いそうだった。とりあえずテキトーに実行してそれをtypescriptの型に変換した。
複数のトップレベルの型を定義するときに、被らないようにしつつ１ファイルに纏める方法が存在しないかもしれない(namespaceはおすすめされていない)

### 統計情報が欲しい

もう少し情報がほしいときには別途APIを呼ばないとだめらしい。

### 50以上の情報を取るにはどうしたら良いんだろう？

nextPageTokenらしいな。あとResponseの型は共通だしまとめるか。この辺が手動になるのはだるいな。

### 参考

- https://developers.google.com/youtube/v3?hl=ja
- https://console.cloud.google.com/apis/api/youtube.googleapis.com/credentials
- https://transform.tools/json-to-typescript
- https://developers.google.com/youtube/v3/docs/search/list
- https://developers.google.com/youtube/v3/docs/channels/list
- https://developers.google.com/youtube/v3/guides/implementation/pagination?hl=ja
- https://chatgpt.com/share/6730e94a-bd98-8001-8fe1-4e717f77c434

## 特定のチャネルの直近一ヶ月の動画を一覧で取得したい

list-videos.ts

### configファイルとして.jsoncのファイルを使いたい

config.jsoncを読み込んで、動画を一覧で取得する。`@std/jsonc`がparserのようなのだけれど、JSON.parseのように特定の型にマッピングされて使うものではないようだ。めんどくさいのでJSONに変換し直して値を取得することにする

### 特定のチャネルの動画のリストを取得したい

どうやらyoutubeのチャンネルのURLの部分はalias?であることもあるらしい。その場合は実際のchannel IDを取得できない。一度search APIを経由して読み込む必要があるみたい。
逆にchannelIdを使って検索してタイトルを取得することもできないらしい。というわけでスマホからURLを渡して使う場合にはこの辺を上手く分岐して情報を抽出する必要があるみたい。

publishedAfterに日付を入れてあげるとそれ以降の動画が手に入るようだ。ところでショート動画と普通の動画が混在する事になってしまうかもしれない。その意図しない動画も含んでしまうかもしれない。

### 参考

- https://developers.google.com/youtube/v3/docs/channels/list?hl=ja
- https://developers.google.com/youtube/v3/docs/playlists/list?hl=ja
- https://www.youtube.com/@meiling_cooking

## 再生リストを作りたい（更新したい）

自分の再生リストを触るのにはどうすれば良いんだろう？なんか軽くドキュメントを見た感じ一気に登録できなそうな感じ？
ChatGPTに雑に聞いた答えがそんな感じだった。マジか。。

https://chatgpt.com/share/67320bd1-0e98-8001-8ae3-b2899b05059f

> YouTube Data APIでは、1回のリクエストで複数の動画を再生リストに追加する方法はサポートされていません。

## 一個ずつしか登録していけないみたい

ついでに再生リストへの登録や更新は1 APIあたり50ユニット使い、１日あたり10000ユニット程度しか操作できないらしい。200個動画をいじったらおしまい。
あ、検索も100ユニット使うのか。

https://developers.google.com/youtube/v3/getting-started?hl=ja#quota

### 参考

- https://developers.google.com/youtube/v3/docs/playlists/insert?hl=ja
- https://developers.google.com/youtube/v3/docs/playlists/update?hl=ja
- https://chatgpt.com/share/67320bd1-0e98-8001-8ae3-b2899b05059f
- https://developers.google.com/youtube/v3/getting-started?hl=ja#quota