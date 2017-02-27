# dtrace govendorがおそすぎるので何をやっているか表示したい

`-v` を付けても静かすぎるのでdtraceで何をやっているか覗く。

```
$ sudo dtrace -l
# とりあえず開いているファイルを覗く
$ sudo dtrace -n 'syscall::open*:entry { printf("%s %s",execname,copyinstr(arg0)); }'
# openだけ
$ sudo dtrace -n 'syscall::open:entry { printf("%s %s",execname,copyinstr(arg0)); }'
```

## memo

- http://www.brendangregg.com/DTrace/dtrace_oneliners.txt

netstatなどで良い気もしてきた。
