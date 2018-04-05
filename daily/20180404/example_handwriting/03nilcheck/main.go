package main

import (
	"fmt"
	"go/types"
)

func main() {
	U := types.Universe
	intT := U.Lookup("int").Type()
	nilT := U.Lookup("nil").Type()

	// intはnilチェックが不要
	fmt.Println("int", types.AssignableTo(nilT, intT))

	// *intはnilチェックが必要
	fmt.Println("*int", types.AssignableTo(nilT, types.NewPointer(intT)))

	// []intもnilチェックが必要
	fmt.Println("*int", types.AssignableTo(nilT, types.NewSlice(intT)))
}
