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
func F() int {
	return 1 + 1
} // xxx
`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	names := []string{"F"}
	for _, name := range names {
		fmt.Println("")
		fmt.Println("<<", name, ">>")
		{
			cmap := ast.NewCommentMap(fset, f, f.Comments)
			for _, c := range cmap.Filter(f.Scope.Lookup(name).Decl.(ast.Node)).Comments() {
				fmt.Printf("%d %d %q\n", c.Pos(), c.End(), c.Text())
			}
		}
	}
}
