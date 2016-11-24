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
