package main

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
)

func main() {
	fset := token.NewFileSet()

	code := `
package p
const x = 10
`
	conf := types.Config{
		Importer: importer.Default(),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}

	// load
	file, err := parser.ParseFile(fset, "p", code, parser.AllErrors)
	pkg, err := conf.Check("p", fset, []*ast.File{file}, nil)
	if err != nil {
		log.Fatal(err)
	}

	// eval
	expr := "x * x"

	tv, err := types.Eval(fset, pkg, token.NoPos, expr)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(expr, "=", tv.Value)

	// y = x * x
	pkg.Scope().Insert(types.NewConst(token.NoPos, pkg, "y", tv.Type, tv.Value))
	expr2 := "x + y"
	tv2, err := types.Eval(fset, pkg, token.NoPos, expr2)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(expr2, "=", tv2.Value)
}
