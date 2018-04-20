package main

import (
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"runtime"
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
		Importer:                 &myimport{internal: importer.For(runtime.Compiler, nil).(types.ImporterFrom)},
		Error: func(err error) {
			log.Println("error -- ", err)
		},
	}
	checker := types.NewChecker(c, fset, types.NewPackage("p", "p"), nil)
	return checker.Files([]*ast.File{f})
}

type myimport struct {
	internal types.ImporterFrom
}

func (m *myimport) Import(path string) (*types.Package, error) {
	return m.ImportFrom(path, "" /* no vendoring */, 0)
}

func (m *myimport) ImportFrom(path, srcDir string, mode types.ImportMode) (*types.Package, error) {
	if mode != 0 {
		panic("mode must be 0")
	}
	log.Println("import -- ", path, srcDir)
	fake := types.NewPackage(path, path)
	return fake, nil
}
