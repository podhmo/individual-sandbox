package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
)

func main() {
	source := `
package p

// F :
func F() int{
	x := 5
	// xxx
	return x + x
}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	dumpPositions(f)
}

func dumpPositions(f ast.Node) {
	if f == nil {
		return
	}
	ast.Inspect(f, func(node ast.Node) bool {
		if node != nil {
			fmt.Printf("%T (%d)\n", node, node.Pos())
		}
		return true
	})
}

