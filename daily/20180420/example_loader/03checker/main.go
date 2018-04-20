package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package p

import "fmt"

type S struct {}
func (s *S) Hello() { fmt.Println("Hello") }

type S2 struct { represent fmt.Stringer; }

type S3 struct {}
func (s *S3) Hello() { fmt.Println("Hello") }

type S4 struct { represent fmt.Stringer; }
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "s.go", source, parser.ParseComments)
	if err != nil {
		return err
	}
	c := &types.Config{
		IgnoreFuncBodies:         true,
		DisableUnusedImportCheck: true,
		Importer:                 &myimport{},
		Error: func(err error) {
			log.Println("error -- ", err)
		},
	}
	p := types.NewPackage("p", "p")
	checker := types.NewChecker(c, fset, p, nil)
	err = checker.Files([]*ast.File{f})
	fmt.Println(p.Scope())
	return err
}

type myimport struct{}

func (m *myimport) Import(path string) (*types.Package, error) {
	return types.NewPackage(path, path), nil
}
