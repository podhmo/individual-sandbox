## python asyncio aqの部分をまともにする

- Groupを再実装したい
- aqでback pressureを実装したい

今、Executorの使いかたが２種類あるのをGroupとExecutorに分けると良いのでは？
あと、Group側でのfutureのリストを常に持つ感じにしたくない。

### hmm

- awaitableなコードを気にせず書きたい
- loopを常に持っているようなtaskが欲しい

### background

for-loopで待ち受けるの正解なんだろうか？
あとkeyboard interruptとか付けるのに不便だったりする。
runner的なものほしくない？objectにガチャガチャ機能をもたせすぎている気がする。

### 疑問

- groupとexecutor分けたものは必要？
- backgroundが本当に必要なのは。。。
- Runner的な機能が欲しいのでは？
- awaitの再実装になる？
- resourceの制限を実装したい


## どういうものを作りたいんだろう？

これをいい感じに作りたかったりする。

- https://k1low.hatenablog.com/entry/2019/01/15/083000

