## やってみたいこと

- 複数のprocessを立ち上げる
- 出力先をstdoutとwebsocketに送る
- それらを複数のコードで実行
- 集約されたログが出る

```
{p0, p1, p2} - websocket > output
```

## python asyncio with stdin, stdout

- https://github.com/python-trio/trio/issues/174
- https://stackoverflow.com/questions/64303607/python-asyncio-how-to-read-stdin-and-write-to-stdout
- https://stackoverflow.com/questions/53779956/why-should-asyncio-streamwriter-drain-be-explicitly-called/53781454#53781454

ライブラリがあるのか

- https://github.com/vxgmichel/aioconsole

ちょっと違うはなしとして subprocessしたとき

https://kevinmccarthy.org/2016/07/25/streaming-subprocess-stdin-and-stdout-with-asyncio-in-python/

## python anyio

便利じゃん

- https://anyio.readthedocs.io/en/stable/tasks.html
- https://anyio.readthedocs.io/en/stable/streams.html
- https://anyio.readthedocs.io/en/stable/fileio.html

## python asyncio

run()? run_until_complete()?

- run_until_complete() は futureを引数に取る

  - coroutine objectを受け取ったときには、内部でTaskに変換してloopにscheduleさせる

- run() は coroutine objectを引数に取る

  - coroutine object以外はValueError

### future, coroutine, task?, awaitable?

- awaitable functionを実行するとcoroutine objectが返る
- awaitで待つのはfuture (not coroutine)
- coroutine objectはloopのcreate_task()越しにtaskになりscheduleされる

  - 実はloopにscheduleしているのはTaskクラスの`__init__()`

- taskはfuture
- futureとは `_asyncio_future_blocking` を持つもの
- (asyncio.ensure_future()はちょっと特殊。以下の条件をチェックして分岐する)

  - coroutine
  - future
  - awaitable

- awaitableとはcoroutineかawaitableなgeneratorかabc.Awaitableのどれか

  - async defを使って作られた関数の戻り値 (native coroutine)
  - types.corountineのデコレータでwrapされたgeneratorの戻り値 (generator based corountine)
  - [`__await__()`](https://www.python.org/dev/peps/pep-0492/#await-expression)を持つオブジェクトもawaitable

    - 実は asyncio.Futureにおいて `__await__ = __iter__` (諸々やって yield self)

### wait() ? gather() ?

asyncio.wait()にcorountineをそのまま受け渡すのはwarningが出るのか

```python
    if any(coroutines.iscoroutine(f) for f in fs):
        warnings.warn("The explicit passing of coroutine objects to "
                      "asyncio.wait() is deprecated since Python 3.8, and "
                      "scheduled for removal in Python 3.11.",
                      DeprecationWarning, stacklevel=2)
```

gather()を使うかcreate_task()するか。
１つだけ待ちたいときはwait()を使う必要があるか。

## python websockets

- https://websockets.readthedocs.io/en/3.0/intro.html

### serve

使い方がよくわかっていない。websockets.serveって何者なんだろう？
websockets.legacy.server.Serveか。

で、これはどういうinterface何だろ？

- `__aenter__()`, `__aexit__()` が実装されている
- `__await__()` が実装されている (謎)
- `websockets.protocol.WebSocketCommonProtocol` が継承されている (asyncio.Protocolが継承されている)

内部が結構独特でcreate serverをwrapしているのか。このコード把握しきれないや。

こんな記事もあるのか。ただちょっと違う気がする。

https://tech.morikatron.ai/entry/2020/07/20/100000

### もう少し真剣に読んで見る

- Serveは websocketのconnectionを作るもの

  - yieldしたときにWebsocketServerが動く
  - loopにtaskが登録される

- 待受けはrun_forever()しちゃうか。Eventを使うか。
- あー、run_until_complete() + run_forever() だとcloseが実行されないね。。


## python asyncio.Protocol

- https://docs.python.org/ja/3/library/asyncio-protocol.html#base-protocol

何が使われるんだろう？

```
asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] data_received(self, data)
    [method] eof_received(self)

asyncio.protocols:BaseProtocol <- builtins:object
    [method] connection_lost(self, exc)
    [method] connection_made(self, transport)
    [method] pause_writing(self)
    [method] resume_writing(self)
```

あ、protocolとtransportってそういうことか。

```
asyncio.transports:Transport <- asyncio.transports:ReadTransport <- asyncio.transports:WriteTransport <- asyncio.transports:BaseTransport <- builtins:object

asyncio.transports:ReadTransport <- asyncio.transports:BaseTransport <- builtins:object
    [method] is_reading(self)
    [method] pause_reading(self)
    [method] resume_reading(self)

asyncio.transports:WriteTransport <- asyncio.transports:BaseTransport <- builtins:object
    [method] abort(self)
    [method] can_write_eof(self)
    [method] get_write_buffer_size(self)
    [method] set_write_buffer_limits(self, high=None, low=None)
    [method] write_eof(self)
    [method] writelines(self, list_of_data)
        [method] write(self, data)

asyncio.transports:BaseTransport <- builtins:object
    [method, OVERRIDE] __init__(self, extra=None)
    [method] close(self)
    [method] get_extra_info(self, name, default=None)
    [method] get_protocol(self)
    [method] is_closing(self)
    [method] set_protocol(self, protocol)
```

https://docs.python.org/ja/3/library/asyncio-protocol.html#loop-subprocess-exec-and-subprocessprotocol

## web socket

前回SSEの事を考えていた。単方向ならそれで良くて双方向ならweb socket。
clientは何かあるかなと思ったらwebsocatが良さそう。

- https://github.com/vi/websocat

MacPortsでもwebsocatでインストールできそう。

## 試してみたもの

- SSEのでもアプリをwsgirefで実装
- cmd.CMDでpdbっぽいシェルを途中で起動
- SSEっぽいハンドラーのシミュレーション

あと何をやってみたいんだろう？

- asyncioでtask queue的なもの
- 個別のtaskをいい感じにgroupingして実行
- 依存をいい感じに管理する方法
- 個別のidを指定せずにactionを実行する方法
