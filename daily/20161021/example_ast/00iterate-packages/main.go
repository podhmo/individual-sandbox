package main

import (
	"fmt"
	"go/parser"
	"go/token"
	"log"
	"os"

	"github.com/davecgh/go-spew/spew"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "go run main.go [target]\n")
		os.Exit(1)
	}
	path := os.Args[1]

	fset := token.NewFileSet()

	pkgs, e := parser.ParseDir(fset, path, nil, 0)
	if e != nil {
		log.Fatal(e)
		return
	}

	for _, pkg := range pkgs {
		fmt.Printf("package %v\n", pkg.Name)
		for fname, f := range pkg.Files {
			fmt.Printf("\tfile: %v\n", fname)
			fmt.Printf("\tdeclarations:\n")
			for _, decl := range f.Decls {
				fmt.Printf("\t\t (%v,%v)\n", decl.Pos(), decl.End())
			}
			spew.Dump(f)
		}
	}
}

// type File struct {
// 	Doc        *CommentGroup   // associated documentation; or nil
// 	Package    token.Pos       // position of "package" keyword
// 	Name       *Ident          // package name
// 	Decls      []Decl          // top-level declarations; or nil
// 	Scope      *Scope          // package scope (this file only)
// 	Imports    []*ImportSpec   // imports in this file
// 	Unresolved []*Ident        // unresolved identifiers in this file
// 	Comments   []*CommentGroup // list of all comments in the source file
// }
