package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"

	"github.com/k0kubun/pp"
)

func main() {
	code0 := `
package p

import "fmt"

// F : 0
func F() int {
	return 10
}

// G : 0
func G() int {
	return 20
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f0.go", code0, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	pp.ColoringEnabled = false
	for _, c := range f0.Comments {
		pp.Println(c)
	}
	fmt.Println("----------------------------------------")
	pp.Println(f0.Scope.Lookup("F"))
	fmt.Println(f0.Scope.Lookup("F").Decl.(ast.Node).Pos())
	fmt.Println("----------------------------------------")

    pp.Prinlnt(ast.NewCommentMap(fset, f0.Scope.Lookup("F").Decl.(ast.Node), f0.Comments).Comments())
}
