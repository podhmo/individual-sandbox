# golang http clientのmockってどうやって書くんだろう？

ここでいうmockはまじめに言うとstubのこと。http requestを実際には投げずに済ませたいという感じ。

- [Testing Your (HTTP) Handlers in Go · go, web, go](https://elithrar.github.io/article/testing-http-handlers-go/)
- [httptest - The Go Programming Language](https://golang.org/pkg/net/http/httptest/)

とりあえず、 `net/http/httptest` を使えば良さそうな感じ？

次は自前で同じような構造を作る方法を整理しておいた方が良い感じはする。


# golang http packageの使い方整理

## server

以下の様な感じで作っていく

- [x] 単純なhello world
- [x] 単純なjson server
- static serve

# make Makefileでprocess idなどを保持しておく方法

この方法が綺麗だった。

```make
start: server.PID

server.PID:
    cd bin && { python server.py & echo $$! > $@; }

stop: server.PID
    kill `cat $<` && rm $<

.PHONY: start stop
```

- [saving PID of spawned process within a Makefile - Stack Overflow](http://stackoverflow.com/questions/23366112/saving-pid-of-spawned-process-within-a-makefile)
