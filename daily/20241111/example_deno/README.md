# Youtubeの再生リストを動的に生成したい

その前にデータを取得しておきたい。別にライブラリとかは要らない気がする。

## API KEYが存在しない

作らないとだめか

## 型の情報がほしい

`npm:@types/youtube` はなんか違いそうだった。とりあえずテキトーに実行してそれをtypescriptの型に変換した。
複数のトップレベルの型を定義するときに、被らないようにしつつ１ファイルに纏める方法が存在しないかもしれない(namespaceはおすすめされていない)

## 統計情報が欲しい

もう少し情報がほしいときには別途APIを呼ばないとだめらしい。

## 50以上の情報を取るにはどうしたら良いんだろう？

nextPageTokenらしいな。あとResponseの型は共通だしまとめるか。この辺が手動になるのはだるいな。

## 参考

- https://developers.google.com/youtube/v3?hl=ja
- https://console.cloud.google.com/apis/api/youtube.googleapis.com/credentials
- https://transform.tools/json-to-typescript
- https://developers.google.com/youtube/v3/docs/search/list
- https://developers.google.com/youtube/v3/docs/channels/list
- https://developers.google.com/youtube/v3/guides/implementation/pagination?hl=ja
- https://chatgpt.com/share/6730e94a-bd98-8001-8fe1-4e717f77c434