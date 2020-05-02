## python prestring改修

- 名前がわからない
- submoduleのenter,leaveを作りたい。


## go python -> go

goのstructをpythonで定義する方法。ただ、これそっくりそのままならコストが掛かるだけなのだよな。

以下が難しそうな点

- 埋め込み
- ポインター

チャネルとかは諦める？
mapやsliceも意外と行けそう。

### import部分どうする？

これを考えるとtypeを作る部分がmoduleを持っていないとダメかもしれない。

### pointer?

もしかしてGoPointerという型は不要では？Optionalで十分？

### 追記

意外といいところいったが、resolverのようなものが状態を持つ必要があるかもしれない。
ファイル毎にprefixの内容は異なるので、uberじゃないけれど、全部qualified importしてあげたほうが良いかもしれない。

あと、metashapeのインターフェイスがやっぱり使いづらい。resolver部分。

## go go-chi

go-chiも手軽に色々できるというわけではないな。

- ../20200501/example_go/10chi-simple/main.go

## go websocket

この辺を理解するのが良さそう。

https://github.com/gorilla/websocket

利用例

- https://github.com/gorilla/websocket/blob/master/examples/command/main.go
- echo

  - https://github.com/gorilla/websocket/blob/master/examples/echo/client.go
  - https://github.com/gorilla/websocket/blob/master/examples/echo/server.go

このあたりが手軽で良いんだろうか？

あとこちらも有名。STDINとSTDOUTをwebsocket上に乗せる

- https://github.com/joewalnes/websocketd
