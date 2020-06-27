package main

import (
	"fmt"
	"go/token"
	"go/types"
	"log"
	"os"

	"github.com/pkg/errors"
	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	fset := token.NewFileSet()
	pkg, err := LoadPackage(fset, args[0])
	if err != nil {
		return err
	}
	fmt.Println(pkg)
	p := pkg.Types // *types.Package

	for _, name := range p.Scope().Names() {
		ob := p.Scope().Lookup(name)
		// fmt.Println(name, ob.Name(), ob.Id(), ob.Type())

		// find base
		base := ob.Type()
		for {
			underlying := base.Underlying()
			if underlying == base {
				break
			}
			base = underlying
		}
		if t, ok := base.(*types.Struct); ok {
			fmt.Println(name, t)
			for i := 0; i < t.NumFields(); i++ {
				field := t.Field(i) // *types.Var
				tag := t.Tag(i)
				embedded := field.Embedded()
				fmt.Println("\t", field.Name(), field.Type(), "exported=", field.Exported(), "Embedded=", embedded, "tag=", tag, "pkg=", field.Pkg().Path())
			}
		}
	}
	return nil
}

// LoadPackage :
func LoadPackage(fset *token.FileSet, importPath string) (*packages.Package, error) {
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedTypes,
	}
	pkgs, err := packages.Load(cfg, importPath)
	if err != nil {
		return nil, err
	}
	if len(pkgs) != 1 {
		return nil, errors.Errorf("something wrong: must be len(pkgs) == 1, but %d", len(pkgs))
	}
	return pkgs[0], nil
}
