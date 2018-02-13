package main

import (
	"fmt"
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
	source := `
package p

type S struct {}

func (s *S) String() string { return "S" }

func F() string { return "f" }
`
	fset := token.NewFileSet()

	file, err := parser.ParseFile(fset, "", []byte(source), parser.ParseComments)
	if err != nil {
		return err
	}

	fmt.Println("find S.String()")
	fmt.Println(file.Scope.Lookup("String"))

	fmt.Println("find F()")
	fmt.Println(file.Scope.Lookup("F"))
	return err
}
