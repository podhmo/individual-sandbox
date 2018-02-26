package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
	"strings"

	"github.com/podhmo/astknife/action/replace"
)

func main() {
	code0 := `
package p

// F :
func F() int {
	return 10
}

// G :
func G() int {
	return 20
}

// H :
func H() int {
	return 30
}
`
	code1 := `
package p

// G :
func G() int {
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

	{
		F := f0.Scope.Lookup("F").Decl.(*ast.FuncDecl)
		fmt.Println("B", F.Pos(), F.End())
		G := f0.Scope.Lookup("G").Decl.(*ast.FuncDecl)
		fmt.Println("B", G.Pos(), G.End())
	}
	cm := ast.NewCommentMap(fset, f0, f0.Comments)
	if _, err := replace.ToplevelToFile(f0, f0.Scope.Lookup("G"), f1.Scope.Lookup("G")); err != nil {
		log.Fatal(err)
	}

	{
		F := f0.Scope.Lookup("F").Decl.(*ast.FuncDecl)
		fmt.Println("A", F.Pos(), F.End())
		G := f0.Scope.Lookup("G").Decl.(*ast.FuncDecl)
		fmt.Println("A", G.Pos(), G.End())
	}

	{
		G := f0.Scope.Lookup("G").Decl.(*ast.FuncDecl)
		fmt.Println("G", cm[G][0].List[0], G.Pos())
		// cm[G][0].List[0].Slash = G.Pos()
		// cm[G][0].List[0].Slash = token.Pos(57)
	}

	// for _, cg := range f0.Comments {
	// 	fmt.Println(fset.Position(cg.Pos()), cg.Pos(), cg.Text())
	// }
	fmt.Println("----------------------------------------")
	var b bytes.Buffer
	printer.Fprint(&b, fset, f0)
	for i, line := range strings.Split(b.String(), "\n") {
		fmt.Printf("%02d: %s\n", i+1, line)
	}
	ast.Fprint(os.Stdout, fset, f0.Decls, nil)
}
