## 00

とりあえずqueueを使った動作を確認してみた。

## 01

queueのインターフェイスをベースにsqsのことを考えて少し変更してみた。
具体的には`q.get()`で返るのがtupleになっている。closureを返した。
これは`q.task_done()`とsqsのdelete messageを同一視したため。message idやreceipt handleなども渡した。

## 02

closureを返す方針だと、複数の値を一度の消すということができないかもしれない。
そんなわけで直接task doneが引数を取るようにしてみた。
(こうなってくるとおそらくwrapされたmessageを扱うということになりそう)

## 03

以前作ったlatestを保持するQueueLikeな構造を真面目にwrapperとして書いてみた。

## 04

以前作ったresume付きのqueueのexampleを03で作った構造を利用して書いてみた。

## 05

tx.protocolを使って内部のencode/decode部分の型を書こうとしてみていた。
(Loadingという名前は全くイケていない）

## 06

queue.Queue上でpickleにencodeするものとしないものとを動かしてみる。
これもほとんどmypyの練習にちかい。

## 07

複数の種類のキューを使い分けるサンプルを書いてみた。

- Queueは最後にsentinelを追加する
- PriorityQueueはput時にpriorityが必要。(sentinelの追加は最初でも最後でも良い)
- LIFOQueueは最初にsentinelを追加する

## 08

共通部分をProtocolという形でくくりだしてみた。encode/decode,on_startup/on_shutdown。
なにかでもひどく紛らわしい感じ。そして常にon_startup(),on_shutdown()を呼ばないといけないというのは分かりづらい。

## 09

利用する側でteardownを持つことにしてみた。これはこれで短くなったがなんだかしっくりと来ない。

## 10

q自体の生成もprotocolが行うようにしてみた。queue factory。queueの生成と一緒にteardownも生成する。
これはまたなんだか微妙だなー。

## 11

結局、metadata(priority)の条件自体は常に現れるのではないか？
この条件をprotocolとして定めておけばまぁどうにかなるのかもしれない？

Messageオブジェクトというものを真剣に検討してみる。

とはいえ、startup,teardownは必ず残る。やっぱりfactoryの形がきれいかも？
messageのdecodeがやりづらいのが不便。

? messageとは ? metadata container?

## 12
