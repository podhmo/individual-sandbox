## go contextでkeyを衝突させない方法

- enumをつくる
- new type
- pointer
- struct{}

- https://qiita.com/Ken2mer/items/654282b2256c610312d3

### stringを直接keyにした場合

一応怒られる

```
should not use basic type untyped string as key in context.WithValue
```

## go relative importの方法

GO11MODULEが有効になっていない場合はrelative importが使えた(GOPATH外)。
有効になっている場合は以下の様なエラーが出る。

```
build github.com/podhmo/individual-sandbox/daily/20181226/example_context/01nggo10: cannot find module for path _VENV/individual-sandbox/daily/20181226/example_context/01nggo10/xxx
```

```console
$ GO111MODULE=off go run main.go
```
- https://github.com/golang/go/issues/26645

### 追記

まじめな対応方法は以下のようなgo.modを書く。自分自身のpackage名なので何でも良い(mainでも)

```
package m
```

そしてimport `./xxx` ではなく `m/xxx` にする。

