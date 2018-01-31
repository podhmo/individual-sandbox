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
	sources := []string{
		`
package p
type Person struct {
	Name string
}
`,
		`
package p
type Person2 struct {
	Name stri
}
`,
	}

	fset := token.NewFileSet()

	var files []*ast.File
	for _, src := range sources {
		file, err := parser.ParseFile(fset, "p", src, parser.ParseComments)
		if err != nil {
			log.Fatal(err)
		}
		files = append(files, file)
	}

	conf := types.Config{
		Importer: importer.Default(),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}
	pkg, err := conf.Check("p", fset, files, nil)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(pkg)
	fmt.Printf("%#v\n", pkg)
}
