package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
)

func main() {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "01shape/main.go", nil, parser.ParseComments)
	fmt.Println(err)
	ast.Print(fset, f.Scope.Lookup("Getter").Decl)
}
