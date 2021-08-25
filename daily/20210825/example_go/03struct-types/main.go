package main

import (
	"fmt"
	"go/types"
)

type Person struct {
	Name string `json:"name"`
}

func main() {
	fooPkg := types.NewPackage("m/foo", "foo")
	personUnderling := types.NewStruct(
		[]*types.Var{types.NewVar(0, nil, "Name", types.Universe.Lookup("string").Type())},
		[]string{`json:"name"`},
	)
	personType := types.NewTypeName(0, fooPkg, "Person", personUnderling)
	person := types.NewNamed(
		personType,
		personUnderling,
		nil,
	)
	fooPkg.Scope().Insert(personType)
	fmt.Println(person)
}
