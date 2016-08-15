# [wip]golang mini-grep

- [これ](https://gist.github.com/podhmo/3cd3c1cd8bb1392dafc2eedc07f3cf35) の課題的なもの

mini-grepという名前ですすめてく

## 0. とりあえず正規表現を使ったコードを書く。
## 1. コマンドライン引数を受け取って受け取ったファイルに対してgrep的な処理をする
## 1.5. 無引数の場合には標準入力を見る
## 2. コマンドライン引数でオプションを取れるようにする(-r で再帰的な探索)
## 3. grepの対象を絞る(--include, --exclude)


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
