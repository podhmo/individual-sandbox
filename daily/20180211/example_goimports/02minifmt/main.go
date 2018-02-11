package main

import (
	"go/format"
	"go/parser"
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

	if err := format.Node(os.Stdout, fset, file); err != nil {
		return err
	}
	return nil
}
