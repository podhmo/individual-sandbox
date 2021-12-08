package main

import (
	"encoding/json"
	"fmt"
	"go/token"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Mode: packages.NeedTypes | packages.NeedSyntax,
		Fset: fset,
	}
	pkgs, err := packages.Load(cfg, "m/foo")
	if err != nil {
		panic(err)
	}
	for _, pkg := range pkgs {
		fmt.Println(pkg)
		fmt.Println(pkg.Types)
		s := pkg.Types.Scope()
		for _, name := range s.Names() {
			ob := s.Lookup(name)
			fmt.Println(ob.Name(), ob.Type())
			json.NewEncoder(os.Stdout).Encode(map[string]interface{}{
				"name":    ob.Name(),
				"expr":    ob.String(),
				"file":    fset.File(ob.Pos()).Name(),
				"pkgName": pkg.Types.Name(),
				"pkgPath": pkg.Types.Path(),
			})
		}
	}
}
