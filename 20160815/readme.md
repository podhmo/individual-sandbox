# [wip]golang 正規表現を使う方法

- [regexp - The Go Programming Language](https://golang.org/pkg/regexp/)

あとで書く

```golang
import "regexp"

regexp.MustCompile()
```

# [wip] golang []byte へのcastでmemory copy

少しだけ興味があった。

- [golang で string を []byte にキャストするとメモリコピーが走ります - Qiita](
- [Goアセンブラ入門 - The Go Programming Language](http://golang-jp.org/doc/asm)


## golangでアセンブリを出力する方法

go toolsの中にあるっぽい。

```golang
$ go tool compile -S file.go
# disable optimization ??
$ go tool compile -S -N file.go
```

まだまだ調べることあるので途中
