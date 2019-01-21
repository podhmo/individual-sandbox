## python asyncio

何を気にしたかったのだっけ？

- asyncio.Queueをchannelのように使ってpipelineを構成できない？

  - たぶんdone channel的なことを気にする必要がある

- 標準入力をasyncioで取得したい場合のきれいな書き方

  - run in executorの使いかたとかfutureの使いかたがようやく把握ができるようになる

- きれいにsubprocessと通信する方法

  - communicateが有効なのはどれ？(bytes,array,memoryview)

- async iteratorの上手な使いかた


## python mypy

- 既存のclassをgenerics化する方法(具体的にはAwaitable[T],Future,Queue)
