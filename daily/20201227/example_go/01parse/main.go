package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
)

func main() {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, os.Args[1], nil, parser.ParseComments)
	if err != nil {
		panic(err)
	}
	for _, decl := range f.Decls {
		decl, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue

		}
		// if decl.Name.Name != "GetAlert" {
		// 	continue
		// }

		fmt.Println(decl.Name.Name)
		ast.Inspect(decl, func(node ast.Node) bool {
			switch node := node.(type) {
			case *ast.CallExpr:
				if fun, ok := node.Fun.(*ast.SelectorExpr); ok {
					if ident, ok := fun.X.(*ast.Ident); ok {
						if ident.Name == "c" {
							fmt.Printf("  - %s.%s()\n", ident.Name, fun.Sel.Name)
						}
					}
				}
			}
			return true
		})
	}
}
