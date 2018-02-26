package main

import (
	"fmt"
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
// F :
func F() int {
	return 10
}

// G :
func G() int {
	return 20
}
`
	code1 := `
package p

// F :
func F() int {
	// xxx
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

	f1Comments := ast.NewCommentMap(fset, f1.Scope.Lookup("F").Decl.(ast.Node), f1.Comments)
	fmt.Println(f1Comments)
	if _, err := replace.ToplevelToFile(f0, f0.Scope.Lookup("F"), f1.Scope.Lookup("F")); err != nil {
		log.Fatal(err)
	}

	printer.Fprint(os.Stdout, fset, f0)
	// fmt.Println("----------------------------------------")
	// fmt.Println("package ", f0.Name.Name)
	// c := &printer.Config{Tabwidth: 8}
	// for _, decl := range f0.Decls {
	// 	c.Fprint(os.Stdout, fset, decl)
	// 	fmt.Println("")
	// }
	// fmt.Println("----------------------------------------")
}
