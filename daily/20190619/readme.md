## too 良い感じのinterface

- consumerの部分の定義はasync generatorを受け取るような形になっていると良い
- 

## asyncio shutdown

- https://gist.github.com/nvgoldin/30cea3c04ee0796ebd0489aa62bcf00a

## asyncio

run_coroutine_threadsafe()の使いみちが思いつかない？

```python
# Create a coroutine
coro = asyncio.sleep(1, result=3)

# Submit the coroutine to a given loop
future = asyncio.run_coroutine_threadsafe(coro, loop)

# Wait for the result with an optional timeout argument
assert future.result(timeout) == 3

# コルーチンから例外が送出された場合、返された Future に通知されます。 これはイベントループの Task をキャンセルするのにも使えます:

try:
    result = future.result(timeout)
except asyncio.TimeoutError:
    print('The coroutine took too long, cancelling the task...')
    future.cancel()
except Exception as exc:
    print(f'The coroutine raised an exception: {exc!r}')
else:
    print(f'The coroutine returned: {result!r}')
```

## websockets python https越し

```python
import ssl
import websockets


ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ssl_context.load_cert_chain(pathlib.Path(__file__).with_name("localhost.pem"))

start_server = websockets.serve(hello, "localhost", 8765, ssl=ssl_context)
```

## websocket example server

```consoole
$ wsdump.py ws://echo.websocket.org/
$ wsdump.py ws://echo.websocket.org/ -v
$ wsdump.py ws://echo.websocket.org/ -vv

$ wsdump.py ws://localhost:8000/ws -vv
--- request header ---
GET /ws HTTP/1.1
Upgrade: websocket
Connection: Upgrade
Host: localhost:8000
Origin: http://localhost:8000
Sec-WebSocket-Key: JzAyXXoJP2HtUQhAQ+qvIA==
Sec-WebSocket-Version: 13


-----------------------
--- response header ---
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: /Or/kBJKbf/a9P9h4EfnyrYxR44=
Date: Wed, 19 Jun 2019 08:41:02 GMT
Server: Python/3.7 websockets/7.0
-----------------------
Press Ctrl+C to quit
> hai
send: b'\x81\x83\xfb\xcd\xe78\x93\xac\x8e'
< text: Message text was: hai
> hello
send: b'\x81\x85\xd4\x8a\xe5\x15\xbc\xef\x89y\xbb'
< text: Message text was: hello
>
```

starletteの雑なhandlerだと閉じた時に以下のようなエラーが出る？

```
starlette.websockets.WebSocketDisconnect: 1006
```

## too.js

実装を覗いてみる？

## python fastapi websocket

- client的なものも
- とりあえずexamplesを
- https://fastapi.tiangolo.com/tutorial/websockets/
- https://qiita.com/south37/items/6f92d4268fe676347160
- https://ohke.hateblo.jp/entry/2019/05/25/180000

:thought-balloon: websocketsを使っているのか。

### 追記

以下の２つがある？

- websocket-client 派
- websockets 派

### 追記

- websocketを覗く機能ってあったっけ？
- そもそもwebsocketの仕様に明るくないかも

### 追記

fastapi側

- starletteのWebsocket オブジェクトなのか
- こちらはreceive_byte,receive_text,receive_jsonがあるので手軽。
- 同様にsendがある。
- 自作している？

手元のclient側

- websockets.connect()をそのまま使う？
- async context managerの返す値は Websockets.client.WebsocketClientProtocol
- 最終的にsyncioのStreamReaderProtocolにたどり着くみたい
- strでもbytesでもlistでもよしなにsend()できるっぽい


```console
$ pyinspect inspect websockets.client:WebSocketClientProtocol
websockets.client:WebSocketClientProtocol <- websockets.protocol:WebSocketCommonProtocol <- asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, *, origin=None, extensions=None, subprotocols=None, extra_headers=None, **kwds)
    [method] handshake(self, wsuri, origin=None, available_extensions=None, available_subprotocols=None, extra_headers=None)
        [static method] process_extensions(headers, available_extensions)
        [static method] process_subprotocol(headers, available_subprotocols)
        [method] write_http_request(self, path, headers)
        [method] read_http_response(self)

websockets.protocol:WebSocketCommonProtocol <- asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] __aiter__(self)
        [method] recv(self)
            [method] ensure_open(self)
    [method, OVERRIDE] __init__(self, *, host=None, port=None, secure=None, ping_interval=20, ping_timeout=20, close_timeout=None, max_size=1048576, max_queue=32, read_limit=65536, write_limit=65536, loop=None, legacy_recv=False, timeout=10)
        [method] client_connected(self, reader, writer)
    [method] close(self, code=1000, reason='')
        [method] fail_connection(self, code=1006, reason='')
            [method] close_connection(self)
                [method] wait_for_connection_lost(self)
        [method] write_close_frame(self, data=b'')
            [method] write_frame(self, fin, opcode, data, *, _expected_state=<State.OPEN: 1>)
                [method] writer_is_closing(self)
                [method] fail_connection(self, code=1006, reason='')
                    [method] close_connection(self)
                        [method] wait_for_connection_lost(self)
                [method] ensure_open(self)
    [property] closed
    [method, OVERRIDE] connection_lost(self, exc)
        [method] abort_keepalive_pings(self)
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
                        [method] write_frame(self, fin, opcode, data, *, _expected_state=<State.OPEN: 1>)
                            [method] writer_is_closing(self)
                            [method] fail_connection(self, code=1006, reason='')
                                [method] close_connection(self)
                                    [method] wait_for_connection_lost(self)
                            [method] ensure_open(self)
                    [method] pong(self, data=b'')
                        [method] ensure_open(self)
                        [method] write_frame(self, fin, opcode, data, *, _expected_state=<State.OPEN: 1>)
                            [method] writer_is_closing(self)
                            [method] fail_connection(self, code=1006, reason='')
                                [method] close_connection(self)
                                    [method] wait_for_connection_lost(self)
                            [method] ensure_open(self)
        [method] keepalive_ping(self)
            [method] ping(self, data=None)
                [method] ensure_open(self)
                [method] write_frame(self, fin, opcode, data, *, _expected_state=<State.OPEN: 1>)
                    [method] writer_is_closing(self)
                    [method] fail_connection(self, code=1006, reason='')
                        [method] close_connection(self)
                            [method] wait_for_connection_lost(self)
                    [method] ensure_open(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
        [method] close_connection(self)
            [method] wait_for_connection_lost(self)
    [method, OVERRIDE] eof_received(self)
    [property] local_address
    [property] open
    [property] remote_address
    [method] send(self, data)
        [method] ensure_open(self)
        [method] write_frame(self, fin, opcode, data, *, _expected_state=<State.OPEN: 1>)
            [method] writer_is_closing(self)
            [method] fail_connection(self, code=1006, reason='')
                [method] close_connection(self)
                    [method] wait_for_connection_lost(self)
            [method] ensure_open(self)
        [method] fail_connection(self, code=1006, reason='')
            [method] close_connection(self)
                [method] wait_for_connection_lost(self)
    [method] wait_closed(self)

asyncio.streams:StreamReaderProtocol <- asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method] __del__(self)
    [method, OVERRIDE] __init__(self, stream_reader, client_connected_cb=None, loop=None)
    [method, OVERRIDE] connection_lost(self, exc)
    [method, OVERRIDE] connection_made(self, transport)
    [method, OVERRIDE] data_received(self, data)
    [method, OVERRIDE] eof_received(self)

asyncio.streams:FlowControlMixin <- asyncio.protocols:Protocol <- asyncio.protocols:BaseProtocol <- builtins:object
    [method, OVERRIDE] __init__(self, loop=None)
    [method] _drain_helper(self)
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

## python websockets

何回かやっている気がするけれど。

基本は

- websockets.serve -- websockets.server.Serve
- websockets.connect -- websockets.client.Connect

@:protocolなどのこと覚えきれていないな。

```console
$ pyinspect list websockets
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
websockets.py35
websockets.py35.client
websockets.py35.server
websockets.py36
websockets.py36.protocol
websockets.server
websockets.speedups
websockets.uri
websockets.utils
websockets.version
```

```console
$ pyinspect inspect websockets.client:Connect
websockets.client:Connect <- builtins:object
    [method] __aenter__(self)
    [method] __aexit__(self, exc_type, exc_value, traceback)
    [method] __await__(self)
    [method, OVERRIDE] __init__(self, uri, *, create_protocol=None, ping_interval=20, ping_timeout=20, close_timeout=None, max_size=1048576, max_queue=32, read_limit=65536, write_limit=65536, loop=None, legacy_recv=False, klass=<class 'websockets.client.WebSocketClientProtocol'>, timeout=10, compression='deflate', origin=None, extensions=None, subprotocols=None, extra_headers=None, **kwds)
    [method] __iter__(self)


$ pyinspect inspect websockets.server:Serve
websockets.server:Serve <- builtins:object
    [method] __aenter__(self)
    [method] __aexit__(self, exc_type, exc_value, traceback)
    [method] __await__(self)
    [method, OVERRIDE] __init__(self, ws_handler, host=None, port=None, *, path=None, create_protocol=None, ping_interval=20, ping_timeout=20, close_timeout=None, max_size=1048576, max_queue=32, read_limit=65536, write_limit=65536, loop=None, legacy_recv=False, klass=<class 'websockets.server.WebSocketServerProtocol'>, timeout=10, compression='deflate', origins=None, extensions=None, subprotocols=None, extra_headers=None, process_request=None, select_subprotocol=None, **kwds)
    [method] __iter__(self)
```

どちらも async iterator

## 色の確認

- asni color sequence
- bold
- 色の反転
- 70番台くらいのやつ

## jupyter notebook

command modeのキーを覚えると楽そう

- https://applingo.tokyo/article/130

- ESC - command mode
- x - remove cell
- b - add cell (below)
- z - undo (remove cell)

:warning: vim vixenをnotebook上では切ると上手く活きそう
defaultだとrun all が存在しないのがツライな。

### 追記

jupyter notebookはchromeで動かした方が良い。primitiveな。
firefoxだと以下の様な問題がある

- ctrl + shift + p -> private modeのwindowが開く
- ctrl + n -> new window
- command modeで b -> buffer (vim vixen)

### 追記

chrome(chromium)でも問題があるな。

- clipboardからのpasteが上手くいかない


## python colorful logger

- 30 : black
- 31 : red
- 32 : green
- 33 : yellow
- 34 : blue
- 35 : purple
- 36 : cyan
- 37 : white

確か少しずらしてもう少しきれいなものがあった記憶。あと背景色反転とか。

### 追記

カラフルな出力にするのはできるのだけれど。そこから先どうするのが良いんだろう？
ぶっちゃけた話stderrをstdoutにした時にどちらがどうかとか判断できるのだっけ？
結局受け取った瞬間に全部stdoutにならない？

### 追記

ログという意味では概ねstdoutで大丈夫。
ところでデバッグのときに狙ったprint文だけを表示したいと言うことがあり、この時にはstderrが欲しくなる。

## python 良い感じのpackage構成

- https://github.com/pypa/sampleproject

何を気にしたい？

- setup.cfg, setup.pyの書き方
- 
