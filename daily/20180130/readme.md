## goの関連のxtools

- https://godoc.org/golang.org/x/tools/go/pointer
- https://godoc.org/golang.org/x/tools/go/loader
- https://godoc.org/golang.org/x/tools/go/ast/astutil
- https://godoc.org/golang.org/x/tools/cmd/gotype
- https://godoc.org/golang.org/x/tools/cmd/digraph


## go structのparse

structのparseにもうちょっと良い方法無いかな?

- go/parser
- go/ast
- go/build
- go/constant
- go/printer
- go/scanner
- go/types
- go/token

依存グラフは [./example_depgo/graph.svg](example_depgo/graph.svg)

見込みがありそうなのはgo/typesとgo/importer

### go/types

go/types.Checkerが動かせると良い

### とても参考になりそうなもの

- https://github.com/golang/example/tree/master/gotypes
