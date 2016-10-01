- [Tornado Web Server — Tornado 4.4.2 documentation](http://www.tornadoweb.org/en/stable/)

## examplesを覗いてみる

- https://github.com/tornadoweb/tornado/tree/stable/demos/

### 00hello.py

- 非同期関連の機能が使われていないので不足

### 01chatdemo

- 複数のファイルが有るので取ってくるのが面倒
- アプリ自体は一昔前っぽい感じがする。(実際2009のものっぽい)
- 以下が独自のもの
-- templateも独自なのlock獲得しないととかがあるからちょっと大変なのかも。。
-- option parser

内部的な話

- template,css付近は気にしなくて良いので観るべきものはchatdemo.pyのみ
- Handlerがviewを束ねるviewsetのようなもの
- かなりasyncioの古い方のapiに似ている
-- asyncio.coroutine -> tornado.gen.coroutine
-- asyncio.futures.Future -> tornado.concurrent.Future
-- asyncio.get_event_loop -> tornado.ioloop.IOLoop.current()
-- asyncio.baseloop.run_until_complete() -> tornado.ioloop.IOLoop.run_sync()

### 03websocket

- handlerがwebsocket用のものがある。(tornado.websocket.WebSocketHandler)
- そもそも特に何か非同期のものが必要なわけじゃないので見るべきものは特に無い

### 04webspider

- 並列性の制御workerの数を追加すれば良いのか
- baseurlだけqueueに追加するという方法がある。
- 前のやつ(supervisor)の実装変えられる？
