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
