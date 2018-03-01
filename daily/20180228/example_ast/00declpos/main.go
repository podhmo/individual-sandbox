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
	source := `package p

// momomom

// F : xxyyz
func F() int {
	// hoho 
	return 1 + 2
}

// mo

`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(f.Scope.Lookup("F").Pos())
	fmt.Println(f.Scope.Lookup("F").Decl.(*ast.FuncDecl).Pos())
	fmt.Println(f.Scope.Lookup("F").Decl.(*ast.FuncDecl).Doc.Pos())
	ast.Inspect(f, func(node ast.Node) bool {
		if node == nil {
			return false
		}
		if cg, ok := node.(*ast.CommentGroup); ok {
			fmt.Println(cg.Pos(), fset.PositionFor(cg.Pos(), false))
			return false
		}
		return true
	})
	fmt.Println(f.Comments)
	fmt.Println(f.Package)
	pp.Println(fset.Position(f.Package), f.Package)
	pp.Println(fset.Position(token.Pos(1)))
	pp.Println(fset.Position(token.Pos(0)))
}

// 35
// 30
// 20
// --
// 38
// 33
// 20
