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
	source := `package p

// momomom

// F : xxyyz
func F() int {
	// hoho
	return 1 + 2
}

// mo

`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	ast.Inspect(f, func(node ast.Node) bool {
		if node != nil {
			if c, ok := node.(*ast.Comment); ok {
				fmt.Println(node)
				c.Text = "// o"
			}
		}
		return true
	})
	printer.Fprint(os.Stdout, fset, f)
}
