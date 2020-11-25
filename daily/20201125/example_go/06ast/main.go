package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
)

func main() {
	if err := run(); err != nil {
		panic(err)
	}
}

func run() error {
	source := `
package main

type Params struct {
	Name string
	Age int
}
`
	fset := token.NewFileSet()
	t, err := parser.ParseFile(fset, "x.go", source, parser.ParseComments)
	if err != nil {
		return err
	}
	decl := t.Scope.Lookup("Params").Decl.(*ast.TypeSpec)
	fmt.Println("Name", decl.Name.Name)
	return printer.Fprint(os.Stdout, fset, decl.Type.(*ast.StructType).Fields)
}
