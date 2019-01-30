## emacs bingo

- https://tech.515hikaru.net/2019-01-13-lsp-mode-with-bingo/

```console
$ go get -u -v github.com/saibing/bingo
$ cd $GOPATH/src/github.com/saibing/bingo
$ GO111MODULE=on go install
```

emacsで

```
M-x package-install eglot

;; eglot
(add-to-list 'eglot-server-programs '(go-mode "bingo" "-format-style" "goimports" "-trace"))
```

## mac bashのバージョンを上げる

```console
$ sudo port selfupdate
$ sudo port install bash bash-completion
$ sudo vim /etc/shells
# 以下を追加
# /opt/local/bin/bash
$ chsh -s /opt/local/bin/bash
```

## python asyncio

- mailbox
- create taskしたもののエラーに気づきにくい(typo)

## python asyncio

つなぎ合わせることができるようになるとどうなるんだろう？
- loopを受け取る？
- async関数を受け取る？
- 関数を返す関数にするのは何が嬉しいんだろう？
- 少なくとも消費者などを気にする形でなければ意味がない。

- ネットワークは繋げて行きたい感じはする
- メソッドチェインをするのは嬉しいの(lambdaは嬉しくない)?
- デコレータは便利そうな気がしている。

なにを事前に設定して、何を実行時に設定したいんだろう？
Queueを指定できるのが利点ていうのは確かなのだよなー。
裏側の配線はdecoratorのタイミングで決まってしまうのだけれど。それは嬉しいんだろうか？

何もしなければasync iteratorになるのでは？

## python asynbcio pattern

https://medium.com/python-pandemonium/asyncio-coroutine-patterns-beyond-await-a6121486656f

- recursive coroutines
- (async_timeout?)

task作ってgatherがfun-outになる。

- awaitしてしまうと待ってしまう。そこでensure_future。
- 残ったタスクのお掃除にasyncio.Task.all_tasks()

periodic

- 周期的にじっこうしたい場合には数を数えてensure futureで良いのかたしかに

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
