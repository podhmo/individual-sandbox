# go/packagesで複数のmainを対象にできたか調べる

あるパッケージの呼び出し箇所とそれ用のファイルを探すとき一度に実行できるか調べたかった。


ファイル構造

```
.
├── Makefile
├── bar
│   └── main.go
├── boo
│   └── boo.go
├── foo
│   └── main.go
├── go.mod
├── go.sum
└── inspect
    └── main.go

4 directories, 7 files
```


fooとbarはmain

```console
$ go run ./foo
foo

$ go run ./bar
bar
```

ここでinspect/main.goを実行してみる。これが実行できれば大丈夫。

```console
go run ./inspect ./foo ./bar ./boo
name:	id:"github.com/podhmo/individual-sandbox/daily/20221122/example_go/foo"		ismain:true:	files:[/home/podhmo/ghq/github.com/podhmo/individual-sandbox/daily/20221122/example_go/foo/main.go]
	Foo func github.com/podhmo/individual-sandbox/daily/20221122/example_go/foo.Foo()
	NAME const github.com/podhmo/individual-sandbox/daily/20221122/example_go/foo.NAME untyped string
	main func github.com/podhmo/individual-sandbox/daily/20221122/example_go/foo.main()

name:	id:"github.com/podhmo/individual-sandbox/daily/20221122/example_go/bar"		ismain:true:	files:[/home/podhmo/ghq/github.com/podhmo/individual-sandbox/daily/20221122/example_go/bar/main.go]
	Bar func github.com/podhmo/individual-sandbox/daily/20221122/example_go/bar.Bar()
	NAME const github.com/podhmo/individual-sandbox/daily/20221122/example_go/bar.NAME untyped string
	main func github.com/podhmo/individual-sandbox/daily/20221122/example_go/bar.main()

name:	id:"github.com/podhmo/individual-sandbox/daily/20221122/example_go/boo"		ismain:false:	files:[/home/podhmo/ghq/github.com/podhmo/individual-sandbox/daily/20221122/example_go/boo/boo.go]
	Boo func github.com/podhmo/individual-sandbox/daily/20221122/example_go/boo.Boo()
	NAME const github.com/podhmo/individual-sandbox/daily/20221122/example_go/boo.NAME untyped string
```