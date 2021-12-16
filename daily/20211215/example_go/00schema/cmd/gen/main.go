package main

import (
	"fmt"
	"go/ast"
	"go/types"

	"golang.org/x/tools/go/packages"
)

func main() {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedTypes | packages.NeedTypesInfo | packages.NeedModule,
	}
	pkgs, err := packages.Load(cfg, "m/def", "m/def2")
	if err != nil {
		panic(err)
	}
	var iface *types.Interface
	for _, pkg := range pkgs {
		ob := pkg.Types.Scope().Lookup("Name")
		if ob == nil {
			continue
		}
		iface = ob.Type().Underlying().(*types.Interface)
	}
	for _, pkg := range pkgs {
		for k, v := range pkg.TypesInfo.Defs {
			typ, ok := v.(*types.TypeName)
			if !ok || !k.IsExported() {
				continue
			}
			if !types.Implements(typ.Type(), iface) {
				continue
			}
			spec, ok := k.Obj.Decl.(*ast.TypeSpec)
			if !ok {
				continue
			}
			if _, ok := spec.Type.(*ast.StructType); !ok {
				continue
			}

			fmt.Println(k.Name, typ.Name(), pkg.PkgPath)
		}
	}

}
