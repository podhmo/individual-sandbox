package main

import (
	"fmt"
	"go/types"
)

func main() {
	foo := types.NewPackage("m/foo", "foo")

	fmt.Println("package")
	{
		var v *types.Package = foo
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	fmt.Println("")
	fmt.Println("")
	fmt.Println("types")
	{
		var v types.Type = types.Typ[types.String]
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	{
		var v types.Type = types.Typ[types.Bool]
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	{
		var v types.Type = types.NewMap(types.Typ[types.String], types.NewSlice(types.NewNamed(types.NewTypeName(0, foo, "MessageFunc", types.NewSignature(nil,
			types.NewTuple(types.NewParam(0, nil, "name", types.Typ[types.String])),
			types.NewTuple(types.NewParam(0, nil, "", types.Typ[types.String])),
			false)),
			nil, nil,
		)))
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	fmt.Println("")
	fmt.Println("")
	fmt.Println("object")
	{
		var v types.Object = types.Universe.Lookup("bool")
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	{
		var v types.Object = types.Universe.Lookup("nil")
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	{
		var v types.Object = types.Universe.Lookup("len")
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	{
		var v types.Object = types.NewVar(0, foo, "Foo", types.Typ[types.String])
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
	{
		var v types.Object = types.NewFunc(0, foo, "Hello", types.NewSignature(nil,
			types.NewTuple(types.NewParam(0, nil, "name", types.Typ[types.String])),
			types.NewTuple(types.NewParam(0, nil, "", types.Typ[types.String])),
			false,
		))
		fmt.Printf("	%-40[1]s	%-40[1]v	%+[1]v\n", v)
	}
}
