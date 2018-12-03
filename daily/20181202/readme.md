## mage

mageのコードけっこう参考になる

- flagの使いかた
- execの使いかた

## go やりたいことを整理しておきたい

やりたいことは「goで作られたスクリプトの前後でちょっとした処理を呼び出したい」ということ。
やっていることは以下

- main.goの書き換え
- 書き換え後のコードでmain.goを呼ぶ

### 方法

書き換えた結果を使って実行

- コードを書き換えて、go run
- コードを書き換えて、別名でbuildして、元に戻す
- tmpdirectoryにコードをコピーして、go run

書き換えるときのポイント

- templateをどうする？
- 追加のpackageは？(goimportsに任せる、astutilで頑張る)

ファイルを取り出す方法

- x/tools/loader使う
- parse.Package + go list -f の結果

## LD_PRELOAD

## go stdlib

使いかたを覚えておきたい

- flag
- exec(go-sh)
- https://medium.com/@catatsuy/go%E3%81%A7%E5%A4%96%E9%83%A8%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%A0%E3%82%92exec%E3%81%99%E3%82%8Bpackage%E3%81%AE%E3%83%86%E3%82%B9%E3%83%88%E3%82%92%E3%81%A9%E3%81%86%E3%81%99%E3%82%8B%E3%81%8B-4c01281af928

## go go run

tmpdirにbinaryを生成して実行

```
/tmp/go-build312950127/b001/exe/main
```

## go goa

- https://github.com/goadesign/goa

v1

```
cd $GOPATH/src/goa-adder
goagen bootstrap -d goa-adder/design
```

v2

```
cd $GOPATH/src/calcsvc
goa gen calcsvc/design
```

### main.go の生成

```
goa gen github.com/goadesign/goa/examples/calc/design --debug
```

`goa538526107:` のようなディレクトリを作って、そこでbuildする

```
goa538526107/
├── goa
└── main.go

0 directories, 2 files
```

## go mage

- https://github.com/magefile/mage

### main.go の生成

`mage_output_file.go` を作る。 `// +build ignore` 付きのmainを作ってgo run

```
mage -init
# edit
mage -keep ls
```

### go listの使いかた

mageで何かしてる

```go
	out, err := outputDebug(gocmd, "list", "-f", "{{.Dir}}||{{.Name}}", importpath)
```

https://budougumi0617.github.io/2018/09/21/package-dependencies-with-go-list-and-build-tags/

### +build mageの意味ってなんだっけ？

タスクの対象となるような関数を取り出すために使われている。`go list` 経由

## go monkey

- https://github.com/bouk/monkey
