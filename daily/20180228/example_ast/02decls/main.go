package main

import (
	"fmt"
	"go/parser"
	"go/token"
	"log"

	"github.com/k0kubun/pp"
)

func main() {
	source := `package p

type S struct {}
type S2 struct {}

type (
	S3 struct{}
	S4 struct{}
)
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	pp.ColoringEnabled = false
	pp.Println(f)
	fmt.Println(len(f.Decls))
	fmt.Printf("%T\n", f.Decls[0])
	fmt.Printf("%T\n", f.Decls[1])
	fmt.Printf("%T\n", f.Decls[2])
	pp.Println(f.Decls[2])
}
