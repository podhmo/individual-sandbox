package main

import (
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/ast/astutil"
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

	c := &printer.Config{Mode: printer.TabIndent, Tabwidth: 8}
	return c.Fprint(os.Stdout, fset, findDecl(file, "User"))
}

func findDecl(file *ast.File, name string) ast.Node {
	node := file.Scope.Lookup("User").Decl.(ast.Node)
	var decl ast.Node
	astutil.Apply(file, func(cursor *astutil.Cursor) bool {
		if cursor.Node() == node {
			decl = cursor.Parent()
			return false
		}
		return true
	}, nil)
	return decl
}
