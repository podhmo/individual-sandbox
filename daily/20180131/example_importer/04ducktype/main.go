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
	sources := []string{
		`
package p
type S string
type A = string
type I = int
`,
	}

	fset := token.NewFileSet()

	var files []*ast.File
	for _, src := range sources {
		file, err := parser.ParseFile(fset, "p", src, parser.ParseComments)
		if err != nil {
			log.Fatal(err)
		}
		files = append(files, file)
	}

	conf := types.Config{
		Importer: importer.Default(),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}

	pkg, err := conf.Check("p", fset, files, nil)
	if err != nil {
		log.Fatal(err)
	}

	tv, err := types.Eval(fset, pkg, token.NoPos, `"foo"`)

	fmt.Printf("%#v\n", tv.Type)
	fmt.Printf("%#v\n", pkg.Scope().Lookup("S").Type())
	fmt.Printf("%#v\n", pkg.Scope().Lookup("A").Type())
	fmt.Println("assignable?", types.AssignableTo(tv.Type, tv.Type))
	fmt.Println("assignable?", types.AssignableTo(tv.Type, pkg.Scope().Lookup("A").Type()))
	fmt.Println("assignable?", types.AssignableTo(pkg.Scope().Lookup("A").Type(), tv.Type))
}
