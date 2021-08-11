package main

import (
	"fmt"
	"go/types"
	"log"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run() error {
	cfg := &packages.Config{Mode: packages.NeedDeps | packages.NeedSyntax | packages.NeedTypesInfo | packages.NeedTypes | packages.NeedImports}
	pkgs, err := packages.Load(cfg, "m/model")
	if err != nil {
		return err
	}
	if packages.PrintErrors(pkgs) > 0 {
		return fmt.Errorf("something wrong")
	}
	for _, pkg := range pkgs {
		s := pkg.Types.Scope()
		for _, name := range s.Names() {
			ob := s.Lookup(name)
			t := ob.Type()
			if err := handleType(t, name); err != nil {
				return err
			}
		}
	}
	return nil
}

func handleType(t types.Type, name string) error {
	switch t := t.(type) {
	case *types.Struct:
		for i := 0; i < t.NumFields(); i++ {
			field := t.Field(i)
			fmt.Printf("%s\t%s\t%s\n", name, field.Name(), field.Type())
		}
		return nil
	case *types.Named:
		return handleType(t.Underlying(), name)
	default:
		return fmt.Errorf("name %s, unexpected type %T, %+v", name, t, t)
	}
}
