package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/ast/astutil"
)

func run() error {
	code0 := `
package p
type S struct {}
func (s *S) Hello() string {
	return "Hello"
}
type T struct {}
`
	code1 := `
package p
type S struct {
	Name string
}
`

	fset := token.NewFileSet()
	f0, err := parser.ParseFile(fset, "f0", code0, parser.ParseComments)
	if err != nil {
		return err
	}
	f1, err := parser.ParseFile(fset, "f1", code1, parser.ParseComments)
	if err != nil {
		return err
	}

	fmt.Println("----------------------------------------")
	printer.Fprint(os.Stdout, fset, f0)
	fmt.Println("----------------------------------------")
	printer.Fprint(os.Stdout, fset, f1)
	fmt.Println("----------------------------------------")

	var x ast.Node
    // collect decl sureba
	ast.Inspect(f1, func(n ast.Node) bool {
		if decl, ok := n.(*ast.GenDecl); ok {
			if spec, ok := n.(*ast.TypeSpec); ok {
				if spec.Name.Name == "S" {
					x = spec
				}
			}
			return false
		}
		return true
	})
	// or
	// f0.Decls[0] = f1.Decls[0]

	fmt.Printf("%#v\n", f0.Decls)
	printer.Fprint(os.Stdout, fset, f0)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
