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
type P struct {
	Name string
}
func (p *P) String() string {
	return p.Name
}

type P2 = P
type P3 P
type P4 = P
type P5 P
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

	names := []string{"P", "P2", "P3", "P4", "P5"}
	for _, name := range names {
		p := pkg.Scope().Lookup(name)
		fmt.Println("P is ", p)

		{
			addressable := true
			obj, idx, indiret := types.LookupFieldOrMethod(p.Type(), addressable, pkg, "Name")
			fmt.Println("P has Name? ", obj, idx, indiret)
		}

		{
			addressable := false
			obj, idx, indiret := types.LookupFieldOrMethod(p.Type(), addressable, pkg, "Name")
			fmt.Println("P has Name? ", obj, idx, indiret)
		}

		{
			addressable := true
			obj, idx, indiret := types.LookupFieldOrMethod(p.Type(), addressable, pkg, "String")
			fmt.Println("P has String()? ", obj, idx, indiret)
		}

		{
			addressable := false
			obj, idx, indiret := types.LookupFieldOrMethod(p.Type(), addressable, pkg, "String")
			fmt.Println("P has String()? ", obj, idx, indiret)
		}
		fmt.Println("")
	}
}
