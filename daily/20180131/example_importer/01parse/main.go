package main

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
)

func main() {
	fset := token.NewFileSet()
	src := `
package p

// Person : person
type Person struct {
	Name string
	Age int
}
`
	var files []*ast.File

	file, err := parser.ParseFile(fset, "p", src, parser.ParseComments)
	if err != nil {
		log.Fatal(err)
	}
	files = append(files, file)

	conf := types.Config{Importer: importer.Default()}
	pkg, err := conf.Check("p", fset, files, nil)

	if err != nil {
		log.Fatal(err)
	}

	for _, file := range files {
		for _, group := range file.Comments {
			for _, comment := range group.List {
				s := comment.Text
				tv, err := types.Eval(fset, pkg, comment.Pos(), s)
				if err != nil {
					log.Fatal(err)
				}
				fmt.Println(tv.Type.String())
				fmt.Println(tv.Value.ExactString())
			}
		}
	}
}
