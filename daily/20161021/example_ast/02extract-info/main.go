package main

import (
	"fmt"
	"go/ast"
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
		for fname, f := range pkg.Files {
			fmt.Println("file: ", fname)
			showAST(f)
			fmt.Println("-----")
			showScope(f.Scope)
			fmt.Println("----------------------------------------")
		}
	}
}

func showAST(f *ast.File) {
	ast.Walk(&Visitor{}, f)
}

func showScope(scope *ast.Scope) {
	for name, ob := range scope.Objects {
		fmt.Printf("\tname: %s\n", name)
		fmt.Printf("\tkind: %s\n", ob.Kind)
		fmt.Printf("\tdata: %+#v\n", ob.Data)
		switch v := ob.Decl.(type) {
		case *ast.ValueSpec:
			fmt.Println("\tvaluespec:", v)
		case *ast.TypeSpec:
			fmt.Println("\ttypespec:", v)
		default:
			fmt.Println("noop")
		}
		spew.Dump(ob)
	}
}

// // The list of possible Object kinds.
// const (
// 	Bad ObjKind = iota // for error handling
// 	Pkg                // package
// 	Con                // constant
// 	Typ                // type
// 	Var                // variable
// 	Fun                // function or method
// 	Lbl                // label
// )

type Visitor struct {
}

func (v *Visitor) Visit(node ast.Node) ast.Visitor {
	if node != nil {
		switch node := node.(type) {
		case *ast.TypeSpec:
			fmt.Println("\tT ", node.Type, " ", node.Name)
		case *ast.ValueSpec:
			fmt.Println("\tV ", node.Type, " ", node.Names, " ", node.Values)
		}
	}
	return v
}
