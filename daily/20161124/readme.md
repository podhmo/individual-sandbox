# growl notifyのかわりをubuntu上でやる

- libnotify
- https://github.com/maerlyn/go-twitter-libnotify

# firefox copy.js

requirements

- vimperator is installed

copy.js
https://gist.github.com/podhmo/893357200a8cfa78514705a1521c3989

# golang objectをkeyにしたmap

golangは値を比較するので大丈夫。


```go
// KeyType :
type KeyType string

// KeyType
const (
	KeyTypeA = KeyType("a")
	KeyTypeB = KeyType("b")
)

type keys struct {
	X    string
	Y    string
	Type KeyType
}
```

ただし、もちろん、pointerがあるとだめ。参照のアドレスは変わるので。

```go
type keys struct {
	X    string
	Y    *string
	Type KeyType
}
```

# golang 値の配列からpointerの配列へのコピー

まじめにスコープを考えてみると当たり前といえば当たり前だけれど。

まちがい

```go
xs := []int{1, 2, 3}
ys := make([]*int, len(xs))
for i, x := range xs {
    ys[i] = &x
}
```

ただしい

```go
xs := []int{1, 2, 3}
ys := make([]*int, len(xs))
for i, x := range xs {
    x := x
    ys[i] = &x
}
```

# python dictのkey,valueの文字列の先頭の大文字を小文字にしたい

はい

```python
import json
import sys


def untitle(s):
    if not s:
        return s
    return "{}{}".format(s[0].lower(), s[1:])


def rec(d):
    if isinstance(d, dict):
        return {untitle(k): rec(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [rec(x) for x in d]
    elif hasattr(d, "title"):
        return untitle(d)
    else:
        return d

json.dump(rec(json.load(sys.stdin)), sys.stdout, indent=2, ensure_ascii=False)
```
