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

func Hello() string { return "string"}
func (s *S) Hello() string { return "string"}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "f0.go", source, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}

	pp.ColoringEnabled = false
	fmt.Println(len(f.Decls))
	for name, ob := range f.Scope.Objects {
		fmt.Printf("name %s, kind=%+v, decl=%+v, type=%+v, data=%+v \n", name, ob.Kind, ob.Decl, ob.Type, ob.Data)
	}
}
