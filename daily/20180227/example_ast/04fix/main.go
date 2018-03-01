package main

// hmm
import (
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"github.com/podhmo/astknife/action/replace"
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
	code1 := `
package p
// F : 1
func F() int {
	x := 5
	return x + x
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f0.go", code0, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	f1, err := parser.ParseFile(fset, "f1.go", code1, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	if _, err := replace.ToplevelToFile(f0, f0.Scope.Lookup("F"), f1.Scope.Lookup("F")); err != nil {
		log.Fatal(err)
	}

	f0.Comments = append(
		[]*ast.CommentGroup{
			{List: []*ast.Comment{{Slash: token.Pos(int(f1.Scope.Lookup("F").Decl.(*ast.FuncDecl).Pos()) - 1), Text: f0.Comments[0].List[0].Text}}},
			{List: []*ast.Comment{{Slash: token.Pos(int(f1.Scope.Lookup("F").Decl.(*ast.FuncDecl).End()) - 1), Text: "//"}}},
		},
		f0.Comments[1:]...,
	)

	printer.Fprint(os.Stdout, fset, f0)
}
