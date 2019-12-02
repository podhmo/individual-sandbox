package main

import (
	"go/ast"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/inspect"
	"golang.org/x/tools/go/analysis/singlechecker"
	"golang.org/x/tools/go/ast/inspector"
)

var Analyzer = &analysis.Analyzer{
	Name:     "funcdecl",
	Doc:      `find function declarations`,
	Requires: []*analysis.Analyzer{inspect.Analyzer},
	Run:      run,
}

func run(pass *analysis.Pass) (interface{}, error) {
	inspect := pass.ResultOf[inspect.Analyzer].(*inspector.Inspector)

	inspect.Preorder([]ast.Node{
		(*ast.FuncDecl)(nil), // 関心のあるノードの種類の値を列挙する（この例では関数定義のみ）
	}, func(node ast.Node) {
		f := node.(*ast.FuncDecl)
		pass.Reportf(f.Pos(), `found %s`, f.Name)
	})

	return nil, nil
}

func main() {
	singlechecker.Main(
		Analyzer,
	)
}
