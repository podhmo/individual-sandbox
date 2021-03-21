package main

import (
	"fmt"
	"go/token"
	"go/types"
)

func main() {
	main := types.NewPackage("main", "main")
	pkg := types.NewPackage("m/foo", "foo")
	pkg.SetImports([]*types.Package{types.NewPackage("fmt", "fmt")})

	s := pkg.Scope()
	s.Insert(types.NewTypeName(token.NoPos, pkg, "Foo", types.Typ[types.String]))

	fmt.Println(pkg)
	for _, imported := range pkg.Imports() {
		fmt.Println("  - ", imported)
	}
	fmt.Println("----------------------------------------")
	fmt.Println(pkg.Scope().Lookup("Foo"))
	fmt.Println("----------------------------------------")
	fmt.Println(types.TypeString(
		types.Typ[types.String],
		RelativeTo(main)),
	)
	fmt.Println(types.TypeString(
		types.NewNamed(types.NewTypeName(token.NoPos, pkg, "Foo", types.Typ[types.String]), nil, nil),
		RelativeTo(main)),
	)
	fmt.Println(types.TypeString(
		types.NewNamed(types.NewTypeName(0, pkg, "Hello", types.NewSignature(nil,
			types.NewTuple(types.NewVar(0, nil, "name", types.Typ[types.String])),
			types.NewTuple(types.NewVar(0, nil, "", types.Typ[types.String])),
			false)),
			nil, nil,
		),
		RelativeTo(main)),
	)
	fmt.Println(types.TypeString(
		types.NewSignature(nil,
			types.NewTuple(types.NewVar(0, nil, "foo", types.NewNamed(types.NewTypeName(token.NoPos, pkg, "Foo", types.Typ[types.String]), nil, nil))),
			types.NewTuple(types.NewVar(0, nil, "", types.Typ[types.String])),
			false),
		RelativeTo(main)),
	)
	fmt.Println(types.TypeString(
		types.NewSignature(nil,
			types.NewTuple(types.NewVar(0, nil, "foo", types.NewNamed(types.NewTypeName(token.NoPos, pkg, "Foo", types.Typ[types.String]), nil, nil))),
			types.NewTuple(types.NewVar(0, nil, "", types.Typ[types.String])),
			false),
		RelativeTo(pkg)),
	)
}

func RelativeTo(pkg *types.Package) types.Qualifier {
	if pkg == nil {
		return nil
	}
	return func(other *types.Package) string {
		if pkg == other {
			return "" // same package; unqualified
		}
		return other.Name()
	}
}
