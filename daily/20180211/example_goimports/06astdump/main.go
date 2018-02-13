package main

import (
	"go/ast"
	"go/parser"
	"go/token"
	"log"
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

type (
User2 struct {Name string; Age int;}
User3 struct {Name string; Age int;}
)
`
	fset := token.NewFileSet()

	file, err := parser.ParseFile(fset, filename, input, parser.ParseComments)
	if err != nil {
		return err
	}
	return ast.Print(fset, file)
}
