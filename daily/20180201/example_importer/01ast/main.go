package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

func main() {
	fset := token.NewFileSet()

	code := (`
package p
type S struct {
	Name string ` + "`" + `json:"name"` + "`" + `
	Age int ` + "`" + `json:"age"` + "`" + `
	i  int
}
`)
	file, err := parser.ParseFile(fset, "p", code, parser.AllErrors)
	if err != nil {
		log.Fatal(err)
	}

	{
		fmt.Println("go/printer")
		fmt.Println("----------------------------------------")
		printer.Fprint(os.Stdout, fset, file)
		fmt.Println("----------------------------------------")
	}
	{
		fmt.Println("ast")
		fmt.Println("----------------------------------------")
		ast.Print(fset, file)
		fmt.Println("----------------------------------------")
	}
}
