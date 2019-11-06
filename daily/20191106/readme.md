## tblsを使ってみる

- https://github.com/k1LoW/tbls

```console
go get -v github.com/k1LoW/tbls
```

## metashape python

色々と途中だった気がする

- databaseからの入力
- walkerを良い感じにする方法

それぞれmysql,postgresqlあたりからの入力とwalkerだけでなく欲しいのはgraphでは？みたいな発想になったような記憶。

あと何がしたかったのだっけ？

### 追記

もう少し細かな部分の調整を気にしている？

- field + default値なし (dataclassesはmypy対応？)
- 継承
- metadataのハンドリング

### 追記

けっこうすぐに予約語の対応が欲しくなった。これどうしようかな。
とりあえずmetadataに入れておくか。

あとtblsのoutを使ってschema情報を取り出してみたけれど。
どうしようかな。。意外とshapeのところのmetadataが爆発してしまうような気もする。

あと、以下の様な関係の中でのresourceをどうやって扱おうかな。。

- shape -- 型
- resource -- shapeの構造を持ったdataset (CSVとか想像してもらえれば良い)
