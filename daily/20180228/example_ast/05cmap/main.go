package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
)

func main() {
	source := `package p

// before

// S :
type S struct {
	// Name :
	Name string
} // hmm

// after

// Hello :
func Hello() string {
	// in hello
	return "string"
} // hmm
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	names := []string{"S", "Hello"}
	for _, name := range names {
		fmt.Println("")
		fmt.Println("<<", name, ">>")
		{
			cmap := ast.NewCommentMap(fset, f, f.Comments)
			for _, c := range cmap.Filter(f.Scope.Lookup(name).Decl.(ast.Node)).Comments() {
				fmt.Printf("%d %d %q\n", c.Pos(), c.End(), c.Text())
			}
		}
		fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
		{
			node := f.Scope.Lookup(name).Decl.(ast.Node)
			for _, c := range Collect(f.Comments, node.Pos()-1, node.End()) {
				fmt.Printf("%d %d %q\n", c.Pos(), c.End(), c.Text())
			}
		}

		fmt.Println("----------------------------------------")
	}

	{
		cmap := ast.NewCommentMap(fset, f.Decls[0], f.Comments)
		for _, c := range cmap.Comments() {
			fmt.Printf("%d %d %q\n", c.Pos(), c.End(), c.Text())
		}
	}
	fmt.Println("----------------------------------------")
	{
		fmt.Println(f.Decls[0].(*ast.GenDecl).Lparen)
	}
}

// Collect :
func Collect(comments []*ast.CommentGroup, pos, end token.Pos) []*ast.CommentGroup {
	r := make([]*ast.CommentGroup, 0, len(comments))

	for i, c := range comments {
		if c.Pos() >= pos {
			for ; i < len(comments); i++ {
				if comments[i].Pos() >= end {
					return r
				}
				r = append(r, comments[i])
			}
		}
	}
	return r
}
