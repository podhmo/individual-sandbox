package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"sort"
)

func main() {
	source := `
package p

import (
	"fmt" // comment in import specs
)

// S :
type S struct {
	// Name :
	Name string  // name
	// Value :
	Value int  // value
} // end of S

// top level comment

// F :
func F(x, y int) int {
	if x > 0 {
		// x is must be positive
		fmt.Println("hmm")
	}
	if y > 0 {
		// y is must be positive
		fmt.Println("hmmmmm")
	}
	return x + y
}
`

	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("*** all comments ***")
	for _, cg := range f.Comments {
		fmt.Printf("(%d, %d) %q\n", cg.Pos(), cg.End(), cg.Text())
	}

	fmt.Println("----------------------------------------")

	var comments []*ast.CommentGroup
	ast.Inspect(f, func(node ast.Node) bool {
		if node != nil {
			if x, ok := node.(*ast.CommentGroup); ok {
				comments = append(comments, x)
			}
		}
		return true
	})
	sort.Slice(comments, func(i, j int) bool { return comments[i].Pos() < comments[j].Pos() })
	fmt.Println("*** comments from children ***")
	for _, cg := range comments {
		fmt.Printf("(%d, %d) %q\n", cg.Pos(), cg.End(), cg.Text())
	}
}
