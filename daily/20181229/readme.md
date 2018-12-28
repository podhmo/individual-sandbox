## go list タグによる分岐

TOOD

- go listでタグを使って依存関係を見る
- go.modとかはどうなるんだろう？

```console
$ 
```

例にgithub.com/google/go-cloudを使ってみる

```
$ vgo get gocloud.dev
```

## go go generateの探索のコスト

そこそこ大きめのパッケージで以下２つのコマンドの実行時間を調べてみる。

```console
$ time go generate ./...
$ time for i in $(git grep -l go:generate | grep -v vendor | xargs -I{} dirname {} | sort -u); do (cd $i; go generate); done
```

両者は共に2.1~2.3sくらいだった代わりはなさそう？

### go generateで使える変数は？

```console
$go help generate | grep -P "^	+\\$" -A 2
	$GOARCH
		The execution architecture (arm, amd64, etc.)
	$GOOS
		The execution operating system (linux, windows, etc.)
	$GOFILE
		The base name of the file.
	$GOLINE
		The line number of the directive in the source file.
	$GOPACKAGE
		The name of the package of the file containing the directive.
	$DOLLAR
		A dollar sign.
```

## goで依存しているpackageを雑に調べる方法

```
$ go list -deps ./... | grep -v vendor

## 標準ライブラリを取り除く
$ go list -f "{{if not .Standard}}{{.ImportPath}}{{end}}" -deps ./...
```
