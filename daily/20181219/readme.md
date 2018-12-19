## go vgo

- https://github.com/golang/go/wiki/vgo

```console
$ go get -v golang.org/x/vgo
$ touch go.mod
$ vgo build  # generate go.mod, go.sum
```

依存しているpackageを見る

```console
$ vgo list -m all
```

test

```console
$ vgo test ./...
```

## go guru

そういえば、guruを使ったことがない


## firefox shortcut

location barーに移動できなくて不便

- ctrl + l -> location bar
- ctrl + k ~> 検索ボックス
- ctrl + shift + k -> web console
- ctrl + u -> source

## nuxt

- https://ja.nuxtjs.org/


## go analysisについて

install

```console
$ go get -v golang.org/x/tools/go/analysis
$ golang.org/x/tools/go/analysis
# vet
$ go get -v golang.org/x/tools/go/analysis/cmd/vet
```

知りたいことは何だろう？

- interface的なもの(値の受け渡しどうしているか?)
- `--vettool` の仕組み

### 追記

golang.org/x/tools/go/analysis/unitcheckerのunitchecker.Mainを呼んでいるだけ

```go
func Main(analyzers ...*analysis.Analyzer) {
...
}
```

肝はunitchecker.goとunitchecker112.goっぽいな
go/importerのForCompiler

defaultは以下のようなコードに

```
var importerForCompiler = func(_ *token.FileSet, compiler string, lookup importer.Lookup) types.Importer {
	// broken legacy implementation (https://golang.org/issue/28995)
	return importer.For(compiler, lookup)
}
```

go1.12からは

```
func init() {
	importerForCompiler = importer.ForCompiler
}
```

実際のMainの実装を見るとgo/typesのConfigに渡すImporterとSizesを与えてCheckを呼び出している感じっぽい。pkg単位？

## go mapのcopyについて

mapのcopyをしたいときの挙動についてまとめる

- `copied := m` -> 参照なので意味ない
- `copy(copied, m)` -> copyは[]Tに対する操作
- `reflect.Copy(copied, m)` -> reflect.Copyはmapに対応していない
- shallowCopy with loop -> shallow copy
- deepCopy with recursion

### copy()について

型のcastも行われるので便利

- `copy(s, a[:5])` とかはできる
- `copy(nil, a)` だと空
- `[]T -> []interface` とかはできない
- `[]byte -> string` とかはできる
