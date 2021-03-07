## GraphQLを。。

- https://www.prisma.io/docs/getting-started/quickstart-typescript

## SQLを。。

- https://medium.com/google-cloud-jp/google-%E3%81%AE-sql-parser-analyzer-%E3%81%AE-zetasql-%E3%81%A8%E3%81%AF%E4%BD%95%E3%81%A7%E3%81%82%E3%82%8B%E3%81%8B-d31e3064bd64
- https://github.com/google/zetasql

## mongo like の 残り

- ok select ($find)
- ok where ($project)
- ok JSON field
- ok? pagination (cursor id)
- join

server

- required

  - tracking who is accessing the information
  - role base permission
  - api doc
  - show table info
  - $find
  - $project

- extend

  - rate limit
  - custom resource

- optional

  - text like response (and json format)
  - window function
  - aggregate function
  - join
  - 関数 `e.g. round(x)`


ところで、SQLのselectやmongoの$projectなども含めて、返すを絞り込む口を追加すると、全てのfieldがnullableになっちゃうよね。。

built-in resource, custom resource

## jsendがわりと好きかもしれない

- https://github.com/omniti-labs/jsend
- fail,error以外要らないというのはたしかにそう

  - 5xxがerror他はfail (bad requestなどをイメージ)
  - もちろんnot foundは404を返す (responseの表現の話しかしていない)

- failにmessageを埋め込みたいときはある


## なぜAPI serverを作りたくなるのか？

４つの制限と3つの機能

- 制限

  - アクセスできる機能を制限したい (e.g. read only)
  - 閲覧可能な情報を制限したい (e.g. responseのフィールドを絞り込み )
  - アクセスする頻度と流量を制限したい (e.g. rate limit)
  - いつ誰がアクセスしたかを記録したい (e.g. log traceability)

- 機能

  - (アクセスを制限した上で)所有者以外にも配信したい
  - アクセス方法を公開したい
  - 機能のアクセスに対するショートカットを用意したい (e.g. BFF endpoint)

これは間にproxyを何故置きたいか？みたいな話にもできる。裏側がRPCで。

## contextual loggingは過渡的な技術なのかもしれない

- そもそも複数のログを吐いてaggregateすれば良いのでは？
- stack traceならぬlog trace
- trace idでgroupingして

ただ、１つにまとまっていると嬉しいときもあるんだけど。集約して見たいときに。


## datasette

- https://github.com/simonw/datasette

## go pagination系の処理とかの作業が途中になっていた

- ../20210226/readme.md
- ../20210224/example_go/09list-api/main.go
- ../20210224/example_go/14mongolike-ast/main.go
- ../20210224/example_go/15eval/main.go

調べられたこと

- mongo likeなJSONを受け取って解釈するAST(?)の確認
- paginationを意識したhandlerの定義
- reflectopenapiを使ったAPI情報の表示

どうしようか悩んでいること

- コードをキレイにする方法
- ネストしたデータの定義 (JSON field?)
ネストした情報をどうしよう？

## RDBMS JSON field

- JSONをfieldとして保存できるのは知っている
- whereの条件にはできない認識だった
- (とはいえ、単に指定してあげればindexを貼ることもできるのでは？)

### postgres

JSONフィールドは `->` と `->>` を使ってfieldを取り出す。
驚くべきことに条件にも使える。

- https://www.postgresqltutorial.com/postgresql-json/

### sqlite

標準では対応していなそう。

- https://www.sqlite.org/json1.html
- https://qiita.com/smith/items/c0bd002666a23a6fe8e4
- https://qiita.com/SoraKumo/items/0e3455684d6f17d36895

djangoの例があった。

- https://medium.com/@philamersune/using-postgresql-jsonfield-in-sqlite-95ad4ad2e5f1

## ECS,terraform

- https://qiita.com/umaibou1126/items/f47e54580035b5aeea80
- https://qiita.com/takoikatakotako/items/bfcaa24fd20f1e17bcf6
