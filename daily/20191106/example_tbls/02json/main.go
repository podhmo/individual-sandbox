package main

import (
	"flag"
	"fmt"
	"go/types"
	"os"
	"reflect"

	_ "database/sql"

	"golang.org/x/tools/go/packages"
)

func main() {
	flag.Parse()

	// Many tools pass their command-line arguments (after any flags)
	// uninterpreted to packages.Load so that it can interpret them
	// according to the conventions of the underlying build system.

	// Need: TypeInfo?
	cfg := &packages.Config{Mode: packages.NeedFiles | packages.NeedSyntax | packages.NeedTypes}
	pkgs, err := packages.Load(cfg, flag.Args()...)
	if err != nil {
		fmt.Fprintf(os.Stderr, "load: %v\n", err)
		os.Exit(1)
	}

	packages.PrintErrors(pkgs) // xxx

	for _, pkg := range pkgs {
		fmt.Println(pkg.ID, pkg.GoFiles)
		objs := Scan(pkg.Types)

		for typ, ob := range objs {
			fmt.Printf("%s: %s\n", typ, ob)
			_ = ob
		}
	}
}

// Scan :
func Scan(pkg *types.Package) map[types.Type][]types.Object {
	r := map[types.Type][]types.Object{}
	s := pkg.Scope()
	for _, name := range reflect.ValueOf(s).Elem().FieldByName("elems").MapKeys() {
		ob := s.Lookup(name.String())
		r[ob.Type()] = append(r[ob.Type()], ob)
	}
	return r
}
