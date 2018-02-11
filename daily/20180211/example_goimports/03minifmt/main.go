package main

import (
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	filename := "./user.go"
	input := `
package p
type User struct {Name string; Age int;}
`
	fset := token.NewFileSet()

	file, err := parser.ParseFile(fset, filename, input, parser.ParseComments)
	if err != nil {
		return err
	}

	ast.SortImports(fset, file)

	c := &printer.Config{Mode: printer.TabIndent, Tabwidth: 8}
	return c.Fprint(os.Stdout, fset, file)
}
