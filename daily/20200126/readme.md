## python minitask

- https://github.com/podhmo/minitask

## python asyncio protocolに合わせるかどうか

- https://docs.python.org/ja/3.5/library/asyncio-protocol.html

protocolもtransportもユーザーに見せることはなさそう。内部で規約として持っていても良いという程度。
基本はstreamsという形になるはず。

reader,writerを一緒に作るとconnectionになる。ただしこれを引数として渡せないので２つ同時は今回は常に使えるというわけではない？

### 概念 streams

streamsは便利オブジェクトで、その下部の実装にprotocolとtransportが使われている。

```python
import asyncio

async def tcp_echo_client(message):
    reader, writer = await asyncio.open_connection(
        '127.0.0.1', 8888)

    print(f'Send: {message!r}')
    writer.write(message.encode())
    await writer.drain()

    data = await reader.read(100)
    print(f'Received: {data.decode()!r}')

    print('Close the connection')
    writer.close()
    await writer.wait_closed()

asyncio.run(tcp_echo_client('Hello World!'))
```

### 概念 protocol, transport

以下２つの概念

- transport
- protocol

> いったん通信チャンネルが確立されると、トランスポートは常に プロトコル インスタンスとのペアを成します。プロトコルはその後さまざまな用途のためトランスポートのメソッドを呼び出します。

つまり、protocolの中でtransportが使われるという関係。ただしこれはprotocolのフィールドとしてtransportを保持するという形ではなく、メソッドに対して引数として渡ってくる事が多い。

#### 追記

いや、そんなことないかも。ふつうにconnection madeみたいなメソッドで初回にtransportが渡される事があるけれど。そのメソッドの中でprotocolの状態にtransportを代入したり便利オブジェクトにwrapして代入したりする。

あんまり好きな形式ではない。

### facade

実際のユーザーはこれらを直接は使わずfacade経由で使う。facadeはcoroutineを生成する関数。

```python
message = 'Hello World!'
coro = loop.create_connection(lambda: EchoClientProtocol(message, loop),
                              '127.0.0.1', 8888)
loop.run_until_complete(coro)
```

ここでEchoClientProtocolはこう。書いておくと勝手に動いてくれるような関数に引数として渡すイメージ。

```python
class EchoClientProtocol(asyncio.Protocol):
    def __init__(self, message, loop):
        self.message = message
        self.loop = loop

    def connection_made(self, transport):
        transport.write(self.message.encode())
        print('Data sent: {!r}'.format(self.message))

    def data_received(self, data):
        print('Data received: {!r}'.format(data.decode()))

    def connection_lost(self, exc):
        print('The server closed the connection')
        print('Stop the event loop')
        self.loop.stop()
```

## python bytearray

asyncio.StreamReaderのbufferはbytearray

- https://docs.python.org/ja/3.5/library/functions.html#bytearray
- https://docs.python.org/ja/3.5/library/stdtypes.html#bytes-methods
- https://docs.python.org/ja/3.5/library/stdtypes.html#binaryseq
- https://docs.python.org/ja/3.5/library/stdtypes.html#typebytearray
- https://docs.python.org/ja/3.5/library/stdtypes.html#printf-style-bytes-formatting

## asyncio drain

- https://docs.python.org/ja/3/library/asyncio-stream.html#asyncio.StreamWriter.drain
