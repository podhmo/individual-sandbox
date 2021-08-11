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
	pkgs, err := packages.Load(cfg, "m/model2")
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
			if err := handleType(t, name, nil); err != nil {
				return err
			}
		}
	}
	return nil
}

func handleType(t types.Type, name string, history []types.Type) error {
	switch t := t.(type) {
	case *types.Struct:
		for i := 0; i < t.NumFields(); i++ {
			field := t.Field(i)
			typ := field.Type()
			if v, ok := typ.(*types.Pointer); ok { // todo: for **p
				typ = v.Elem()
			}
			switch typ.(type) {
			case *types.Named, *types.Struct:
				if err := handleType(typ, field.Name(), append(history, t)); err != nil {
					return err
				}
			case *types.Basic:
					fmt.Printf("%s\t%s\t%s\n", name, field.Name(), typ)
			default:
				fmt.Printf("%s\t%s\t%s\t\t%T\n", name, field.Name(), typ, typ)
			}
		}
		return nil
	case *types.Named:
		return handleType(t.Underlying(), name, append(history, t))
	default:
		return fmt.Errorf("name %s, unexpected type %T, %+v", name, t, t)
	}
}
