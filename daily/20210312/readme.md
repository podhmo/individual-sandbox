## terraform

- resource無しのmain.tfのススメ
- hello world
- moduleを作ってみるのも良いのでは, data/localsに情報を保持しておく, outputで戻り値
- local_fileでアクセサーを作るってあり？

## go sql multi-tenant

- multi tenantを前提にRBAC的な処理のweb apiを作る例
- テスト時には一度しか create database / drop database をしない
- テスト実行時や特定の環境では multi tenant 用の絞り込みが行われない場合エラー

このために、Driverを作成する。
取り出したqueryをsql parserにかける。

## go send event to goroutine as monitoring

- 特定の処理を行うときに、validation的なものを追加したい
- そのための付帯的な依存が増えるのは不毛な感じ
- goroutineとchannelを使ってあげれば良いじゃん？

これの練習をやってみたい。
goroutine leakは怖いし。goroutineのハンドリングは正しくしないと。。
結局ログ出力もしてくれれば良いだけ？
エラーを送信して、ログを出力してくれる何かがあれば良いのでは？

## go traceback

- vendor部分の処理などをtracebackに含めても読みにくいだけでは？
- stack trace中で不要そうな呼び出しを省略できると便利そう
- pcだけあれば、stack traceは取り出せる？
- gp,sp,pcの意味
- (allを取るときのstopTheWorld,startTheWorld)

pc=program counter, sp = stack pointer, gp = (current) goroutine pointer

### lazy stack trace

- runtime.Callersでpcを埋めてくれる
- runtime.GOROOTで現在のgorootが見れる
- CallersFramesでpcからframeを取り出せる
- FuncForPCしてFileLineを取り出しているみたい  https://github.com/pkg/errors/blob/master/stack.go
- sp,gpは使わない(内部的には必要だけれどfindFuncなど不要にしている)

### try and error

- 上手くendpointにつながらない。だるい。
- routerで全てのendpointのlookupに失敗したときのhookが欲しい


## hmm

- https://github.com/joomcode/errorx
- https://github.com/pkg/errors/pull/197/files

## go encoding

- encoding, encoding/json, encoding/text
- 素直に親の階層にinterfaceをもたせるのがキレイではある TextMarshaller
- MarshalJSONがなかった場合にMarshalTextを見に行くのを知らなかった

## sugoi

- https://qiita.com/siida36/items/4c0dbaa07c456a9fadd0
- https://srad.jp/~yasuoka/journal/643631/
- https://note.com/c9912211/n/ndd7589e3218f
- https://drive.google.com/file/d/1gRWg4fW_ieGZKmAz1APfymHf6L3owkBm/view

hmm

- https://qiita.com/ddpmntcpbr/items/739dbb992b5ffac3fc2f
- https://qiita.com/shnchr/items/f7e9aaa5105ba885e8f5
- https://qiita.com/shinmura0/items/5f2c363812f7cdcc8771

一番上のやつ、開発スキルはこれくらいで必要十分で、残りは全体をいい感じにすることに振っていると言う感じ？最近はすごいなー。
動かす分にはそれで良いしそれで良いのかもしれないみたいな気持ち。
