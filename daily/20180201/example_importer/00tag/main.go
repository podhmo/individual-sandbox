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

	code := (`
package p

import "time"

type S struct {
	Name string ` + "`" + `json:"name"` + "`" + `
	Age int ` + "`" + `json:"age"` + "`" + `
    Birth time.Time ` + "`" + `json:"birth"` + "`" + `
	i  int
}
`)
	conf := types.Config{
		Importer: importer.Default(),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}

	file, err := parser.ParseFile(fset, "p", code, parser.AllErrors)
	if err != nil {
		log.Fatal(err)
	}
	pkg, err := conf.Check("p", fset, []*ast.File{file}, nil)
	if err != nil {
		log.Fatal(err)
	}

	S := pkg.Scope().Lookup("S")
	internal := S.Type().Underlying().(*types.Struct)

	for i := 0; i < internal.NumFields(); i++ {
		jsonname, found := reflect.StructTag(internal.Tag(i)).Lookup("json")
		field := internal.Field(i)
		fmt.Printf("%v (exported=%t, jsonname=%s, found=%t)\n", field, field.Exported(), jsonname, found)
	}
}
