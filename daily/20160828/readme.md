# golang idiomatic tricks

- [Idiomatic Go Tricks](http://go-talks.appspot.com/github.com/matryer/present/idiomatic-go-tricks/main.slide#1)

テキトウに写してみる。実行はできなそう。

# golang golang.org/x/time/rate

golang.org/x系のライブラリ結構ある。今回はlimitの話。

```go

import golang.org/x/time/rate

ctx := context.Background()
n := rate.Every(time.Second / time.Duration(M))
l := rate.NewLimitter(n, M)

if err := l.Wait(ctx); err != nil {
    do_somehing(err)
}
```

- [golang.org/x/time/rateで速度制限を行う - Qiita](http://qiita.com/lufia/items/29bf1aeb0a0fe69d16f0)

# golang type conversion, type switch, type assertion

綺麗な例が思いつかない。
type assertionとtype switchingはdown castの際に使っている感じはする。

type conversion

```go
time.Sleep(time.Duration(1) * time.Second)
```

これはよく使う。


type assertion

```go
ob, ok := v.(<type>)
```

多分 interfaceから 具象型に変換したい時に使う。(down cast)

type switching

```
switch ob := v.(type) {
case myint:
    return hmm()
case int:
    return hmm2()
default:
    return hmm3()
}
```

あとは、別の異なるinterfaceに変換みたいなこともあるかも? Reader的なもので集めたところでCloser的なものを取り出すとか。

# golang もう少しsortのサンプル

custom sort

## reverseの方法

```go
sort.Sort(sort.Reverse(byXXX(xs)))
```

## 少しだけ柔軟なsort

```go
type customSort struct {
	t    []*Track // ここは固定になる
	less func(x, y *Track) bool
}

func (x customSort) Len() int {
	return len(x.t)
}

func (x customSort) Less(i, j int) bool {
	return x.less(x.t[i], x.t[j])
}

func (x customSort) Swap(i, j int) {
	x.t[i], x.t[j] = x.t[j], x.t[i]
}
```
