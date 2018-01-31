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
type X int
type Y X
type Z int
type A string
type PX *X
type PX2 = *X

type F struct {X int; Y int}
type F2 struct {X int; Y int}
type F3 struct {X int; Y *int}
type F4 struct {X int; Y int; z int}
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

	candidates := []struct {
		src string
		dst string
	}{
		{src: "X", dst: "Y"},
		{src: "X", dst: "Z"},
		{src: "X", dst: "A"},

		{src: "X", dst: "PX"},
		{src: "X", dst: "PX"},

		{src: "F", dst: "F2"},
		{src: "F", dst: "F3"},
		{src: "F", dst: "F4"},
		{src: "F2", dst: "F"},
		{src: "F3", dst: "F"},
		{src: "F4", dst: "F"},
	}

	for _, c := range candidates {
		src := pkg.Scope().Lookup(c.src)
		dst := pkg.Scope().Lookup(c.dst)
		fmt.Printf("convertable %v -> %v ? %v\n", src, dst, types.ConvertibleTo(src.Type(), dst.Type()))
	}
	fmt.Println("")
}
