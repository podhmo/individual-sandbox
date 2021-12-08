package main

import (
	"encoding/json"
	"fmt"
	"go/token"
	"go/types"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	fset := token.NewFileSet()
	cfg := &packages.Config{Mode: packages.NeedTypes, Fset: fset}
	pkgs, err := packages.Load(cfg, "m/foo")
	if err != nil {
		panic(err)
	}
	for _, pkg := range pkgs {
		fmt.Println(pkg)
		fmt.Println(pkg.Types)
		s := pkg.Types.Scope()
		iface := s.Lookup("Namer").Type().Underlying().(*types.Interface)
		for _, name := range s.Names() {
			ob := s.Lookup(name)
			if _, ok := ob.Type().Underlying().(*types.Struct); !ok {
				fmt.Println("skip ...", ob.Name(), ob.Type())
				continue
			}
			fmt.Println(ob.Name(), ob.Type(),
				types.Implements(ob.Type(), iface),
				types.Implements(types.NewPointer(ob.Type()), iface),
			)
			json.NewEncoder(os.Stdout).Encode(map[string]interface{}{
				"name": ob.Name(),
				"expr": ob.String(),
				"file": fset.File(ob.Pos()).Name(),
				"pkgName": pkg.Types.Name(),
				"pkgPath": pkg.Types.Path(),
			})
		}
	}
}
