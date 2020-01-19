## python お手軽spinner

curses不慣れ

- https://u7fa9.org/memo/HEAD/archives/2018-12/2018-12-26.rst

## python selectors

そういえば複数fdの監視はselectorsを使えば良くない？

selectors.selectってtimeout渡した方が良いんだろうか？

- Noneのときは、どれか一つ準備できるまで待つ
- timeout指定したら、timeoutまで待つ

Noneで良いかも

### asyncio

SelectorEventLoopはdefaultだしasyncioで待っても良いのでは？

- https://docs.python.org/ja/3/library/asyncio-eventloop.html?highlight=add_reader#asyncio.SelectorEventLoop

あとはadd_reader(),add_writer()

- https://docs.python.org/ja/3/library/asyncio-eventloop.html?highlight=add_reader#asyncio.loop.add_reader

## python multiprocessing / subprocess

- multiprocessingってどこでも使えるっけ？

## python rpc tinyrpc

これ便利かも

- https://github.com/mbr/tinyrpc


### architecture

- https://tinyrpc.readthedocs.io/en/latest/structure.html

イメージ的には、

以下の様な形でprotocolでつながっている。

- RPCClient -<>- RPCProtocol -<> PRCServer

そしてclientはClientTransport、serverはServerTransportが存在
受け取ったメッセージはPRCDispatherでhandling。

#### protocols?

これはファイル名を見てもらうのが一番わかり易い

- tinyrpc/protocols/jsonrpc.py
- tinyrpc/protocols/msgpackrpc.py

#### transport?

こういうこと。

```console
$ pyinspect inspect tinyrpc.transports
tinyrpc.transports:ServerTransport <- builtins:object
    [method] receive_message(self) -> Tuple[Any, bytes]
    [method] send_reply(self, context: Any, reply: bytes) -> None

----------------------------------------
tinyrpc.transports:ClientTransport <- builtins:object
    [method] send_message(self, message: bytes, expect_reply: bool = True) -> bytes

----------------------------------------
```

### どこまで利用可能なんだろう？

個人的にはgeventへの依存はなくて良い気がしている。
代わりにasyncioなどを使いたい。

- protocolsは使いたい
- Dispatcherは?

この２つは素直に使えそう。

