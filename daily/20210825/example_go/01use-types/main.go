package main

import (
	"bytes"
	"fmt"
	"go/types"
)

func main() {
	xxx := types.NewPackage("m/xxx", "xxx")
	main := types.NewPackage("main", "main")

	params := types.NewTuple(types.NewVar(0, xxx, "foo", types.NewNamed(
		types.NewTypeName(0, xxx, "Foo", nil), nil, nil),
	))
	returns := types.NewTuple()
	foo := types.NewSignature(nil, params, returns, false)

	fmt.Println(foo)

	var buf bytes.Buffer
	// types.WriteSignature(&buf, foo, types.RelativeTo(xxx))
	// fmt.Fprintln(&buf, "")
	// types.WriteSignature(&buf, foo, types.RelativeTo(main))
	types.WriteSignature(&buf, foo, Imported(main))
	fmt.Fprintln(&buf, "")
	types.WriteSignature(&buf, foo, Imported(xxx))
	fmt.Println(buf.String())
}

func Imported(here *types.Package) types.Qualifier {
	return func(there *types.Package) string {
		if here == there {
			return ""
		}
		return there.Name()
	}
}
