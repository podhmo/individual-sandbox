package main

import (
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"reflect"
)

func main() {
	fset := token.NewFileSet()

	codeset := []string{
		(`
package p
type S struct {
	Name string ` + "`" + `json:"name"` + "`" + `
	Age int ` + "`" + `json:"age"` + "`" + `
	i  int
}
`),
		(`
package p
type S2 struct {
	Name string ` + "`" + `json:"name"` + "`" + `
	Age int ` + "`" + `json:"age"` + "`" + `
}
`),
	}

	conf := types.Config{
		Importer: importer.Default(),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}

	var files []*ast.File
	for _, code := range codeset {
		file, err := parser.ParseFile(fset, "p", code, parser.AllErrors)
		if err != nil {
			log.Fatal(err)
		}
		files = append(files, file)
	}

	pkg, err := conf.Check("p", fset, files, nil)
	if err != nil {
		log.Fatal(err)
	}

	// fmt.Println(pkg.Scope().String())

	s := pkg.Scope()
	for _, name := range s.Names() {
		ob := s.Lookup(name)
		if internal, ok := ob.Type().Underlying().(*types.Struct); ok {
			fmt.Println(name)
			for i := 0; i < internal.NumFields(); i++ {
				jsonname, found := reflect.StructTag(internal.Tag(i)).Lookup("json")
				field := internal.Field(i)
				fmt.Printf("	%v (exported=%t, jsonname=%s, found=%t)\n", field, field.Exported(), jsonname, found)
			}
			fmt.Println("")
		}
	}
}
