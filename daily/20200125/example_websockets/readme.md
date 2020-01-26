# websocketsのコードの設計を調べる

記憶が確かならsend/recvだけで済むような実装になっていた記憶。

- https://github.com/aaugustin/websockets

ここか。

> Simplicity: all you need to understand is msg = await ws.recv() and await ws.send(msg); websockets takes care of managing connections so you can focus on your application.

## moduleの一覧

雑に見てみる。

```console
$ pyinspect list websockets | grep -v test | grep -v py
websockets
websockets.client
websockets.compatibility
websockets.exceptions
websockets.extensions
websockets.extensions.base
websockets.extensions.permessage_deflate
websockets.framing
websockets.handshake
websockets.headers
websockets.http
websockets.protocol
websockets.server
websockets.speedups
```

んー。protocolとか覗くか。

## protocol

頑張って中覗いた方が早そう。

```console
$ pyinspect inspect websockets.protocol:WebSocketCommonProtocol
websockets.protocol:WebSocketCommonProtocol <- asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] __aiter__(self)
        [method] recv(self)
            [method] ensure_open(self)
    [method, OVERRIDE] __init__(self, *, host=None, port=None, secure=None, timeout=10, max_size=1048576, max_queue=32, read_limit=65536, write_limit=65536, loop=None, legacy_recv=False)
        [method] client_connected(self, reader, writer)
    [method] close(self, code=1000, reason='')
        [method] fail_connection(self, code=1006, reason='')
            [method] close_connection(self)
                [method] wait_for_connection_lost(self)
        [method] write_close_frame(self, data=b'')
            [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
                [method] writer_is_closing(self)
                [method] fail_connection(self, code=1006, reason='')
                    [method] close_connection(self)
                        [method] wait_for_connection_lost(self)
                [method] ensure_open(self)
    [property] closed
    [method, OVERRIDE] connection_lost(self, exc)
    [method, OVERRIDE] connection_made(self, transport)
    [method] connection_open(self)
        [method] transfer_data(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] read_message(self)
                [method] read_data_frame(self, max_size)
                    [method] read_frame(self, max_size)
                    [method] write_close_frame(self, data=b'')
                        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
                            [method] writer_is_closing(self)
                            [method] fail_connection(self, code=1006, reason='')
                                [method] close_connection(self)
                                    [method] wait_for_connection_lost(self)
                            [method] ensure_open(self)
                    [method] pong(self, data=b'')
                        [method] ensure_open(self)
                        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
                            [method] writer_is_closing(self)
                            [method] fail_connection(self, code=1006, reason='')
                                [method] close_connection(self)
                                    [method] wait_for_connection_lost(self)
                            [method] ensure_open(self)
        [method] close_connection(self)
            [method] wait_for_connection_lost(self)
    [method, OVERRIDE] eof_received(self)
    [property] local_address
    [property] open
    [method] ping(self, data=None)
        [method] ensure_open(self)
        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
            [method] writer_is_closing(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] ensure_open(self)
    [property] remote_address
    [method] send(self, data)
        [method] ensure_open(self)
        [method] write_frame(self, opcode, data=b'', _expected_state=<State.OPEN: 1>)
            [method] writer_is_closing(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] ensure_open(self)

asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] __del__(self)
    [method, OVERRIDE] __init__(self, stream_reader, client_connected_cb=None, loop=None)
    [method, OVERRIDE] _get_close_waiter(self, stream)
    [method, OVERRIDE] connection_lost(self, exc)
        [property] _stream_reader
    [method, OVERRIDE] connection_made(self, transport)
        [property] _stream_reader
    [method, OVERRIDE] data_received(self, data)
        [property] _stream_reader
    [method, OVERRIDE] eof_received(self)
        [property] _stream_reader

asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, loop=None)
    [method] _drain_helper(self)
    [method] _get_close_waiter(self, stream)
    [method, OVERRIDE] connection_lost(self, exc)
    [method, OVERRIDE] pause_writing(self)
    [method, OVERRIDE] resume_writing(self)

asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] data_received(self, data)
    [method] eof_received(self)

asyncio.protocols:BaseProtocol <- builtins:object
    [method] connection_lost(self, exc)
    [method] connection_made(self, transport)
    [method] pause_writing(self)
    [method] resume_writing(self)
```

## 知りたかったのは名前

- WebsocketClientProtocol
- WebsocketServerProtocol

まぁ妥当な話かも。

