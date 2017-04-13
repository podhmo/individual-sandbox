# unix port使っているprocess調べる方法(processが開いているportを調べる方法)

netstatが忘れて思い出せない

lsof使う方法

```
$ sudo lsof -n -P -p <pid> | grep TCP
```

netstatの方法

```
$ sudo netstat -anp | grep <process>
```

- http://blog.livedoor.jp/sonots/archives/32637678.html
- http://d.hatena.ne.jp/yohei-a/20100603/1275580956

# golang 色々一気に実行するやつ

- https://github.com/tockins/realize

# golang memory leakの例

- https://husobee.github.io/golang/memory/leak/2016/02/11/go-mem-leak.html
- https://deferpanic.com/blog/understanding-golang-memory-usage/

# golang leak系の調査したい

go tool pprof

- https://stackimpact.com/blog/memory-leak-detection-in-production-go-applications/

go tool trace

- https://making.pusher.com/go-tool-trace/

goappmonitor

- https://github.com/wgliang/goappmonitor

# golang traceback

runtime callerとか使う

- http://sgykfjsm.github.io/blog/2016/01/20/golang-function-tracing/
