## 作業

前回どうだったっけ？

[../../20191023/example_metashape/readme.md](../../20191023/example_metashape/readme.md)

どうやら

- optional, arrayの対応
- dataclasses化
- graphql,jsonschema,openapi辺りを試しに作ってみる

ということをしていたらしい。なるべく手を拡げてできそうなことができるかを模索していた感じだった。

- 型付け

### きになること

- 不要なclassを抑制
- description metadataの対応

最近一番ツライなと思ったのはgoでのclientを作成することだった。
そういう意味ではJSONからこれらが引っ張ってこれると便利。json2pyclass。
まず、output用とinput用を分けたいかもdriversをoutputsに変えるか。

### 追記

- drivers -> outputs
- inputsを作製

考えてみるとhar辺りから作れたほうが良い？application/jsonなのか辺りがわからないのだよなー。
でも自動での何かがしたいわけではないのでそこは気をつけなくちゃかも。
あくまでshapeが手軽なら良いという感じ。

この辺でお茶を濁せそう[help message](json2models.help.md)

```console
$ json2models -m <class name> -s flat -f base <filename>
```

dynamic typingが邪魔だな。。まぁ80%位の感じで使える。

## 追記

ほしかったものが見えてきた。poetryのようなものかも。
initしてプロジェクトを生成する。
resourceを追加したかったらaddする。
(resource,shape,schemaの辺りがたぶん曖昧)

## jsonschemaのtoplevel?

- ファイル名と同じだと良いんじゃない？
- 名前部分を手軽にやれると良い？

## どこから作っていくか？

- jsonrpc辺りが手軽？
- grpcでも良いかもしれない
- ものによってはbaseのschemaだけを生成すればよい？
- いや、designを実行してglue部分を生成する必要はありそう
- 別途sandbox的なrepositoryが欲しいかもしれない?

## http client

ほしいものはなんだろう？

- documentation
- (authentication)
- bulk action
- backoff
- monitoring (logging)
