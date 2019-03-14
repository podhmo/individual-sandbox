## lsp

json用のあれを作るか。

どこにあったかな。一度疎通をclientコードでやったはず。

### 追記

- とりあえずpylsを手元でつなげる。
- clientを作って手元で繋げる
- sub processでpylsを動かして繋げる

tcpで動かす

```console
$ pyls --tcp --port 44444 -v 
```

## python tcp

tcp serverの作り方整理しておこう

(reuse_socketが重要)

- https://docs.python.org/ja/3/library/socketserver.html
- https://docs.python.org/ja/3/library/socket.html
- https://docs.python.org/ja/3/howto/sockets.html

(sendfile使えない？)
(non blockingにする必要ある？)

### replが欲しい(clientで)

- https://docs.python.org/ja/3/library/cmd.html

### 欲を出してasyncio

- https://docs.python.org/ja/3/library/asyncio-protocol.html

まじめにやりたい場合はaiohttpの中などをのぞくべきかも？

https://github.com/aio-libs/aiohttp

### 関係の整理

```
socketserver -> {selectors, socket}
asyncio -> {selectors,socket}
```
