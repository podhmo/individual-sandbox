package main

import (
	"fmt"
	"go/parser"
	"go/token"
	"log"
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

	{
		fmt.Println(f.Pos(), f.End(), f.End()-f.Pos())
	}
	{
		f := fset.File(f.Pos())
		fmt.Println(f.Base(), f.Size())
	}
}
