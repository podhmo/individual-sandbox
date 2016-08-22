# wip golang graceful stop

参考

- [Go 1.7 Release Party in Tokyo -- context パッケージの紹介](http://go-talks.appspot.com/github.com/matope/talks/2016/context/context.slide#1)
- [http.Serverをgracefulに停止させる - Qiita](http://qiita.com/nyamage/items/35feebdb1d39a570930a)

# golang 同じ文字をN回使った文字列を生成

`strings.Repeat()` を使えば良い。

```
strings.Repeat(" ", 10)
```

# golang golang.org/x/net/context

とりあえず以下の様な感じで使えば良いのかな？

```go
import "golang.org/x/net/context"

ctx := context.Background()
ctx, cancel := context.WithCancel(ctx)
ctx, cancel = context.WithTimeout(ctx, time.Duration(6)*time.Second)

go do(ctx)
```

内部では以下のようなものをで管理すれば良い？

```go
func do(ctx context.Context){
    select {
    case <- ctx.Done():
        return // skip
    default:
        do_something()
        return
    }
}
```


# golang time.Ticker

これだとダメ。ticker.Stop()されてもchannelはcloseされないらしい。
([ここ](http://okzk.hatenablog.com/entry/2015/12/01/001924) ただし確かめ方がまだよく分かっていない)

```go
ticker := time.NewTicker(waitTime)
go func() {
    for t := range ticker.C {
        fmt.Println("Tick at", t)
    }
}()

doSomething()
ticker.Stop()
```

selectで頑張る。

```go
ticker := time.NewTicker(waitTime)
stop := make(chan struct{})
go func() {
    loop:
    select {
    case t := <- ticker.C:
        fmt.Println("Tick at", t)
    case <- stop:
        break loop
    }
}()

doSomething()
ticker.Stop()
stop <- struct{}{}
close(stop)
```
