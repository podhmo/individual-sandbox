package main

import (
	"fmt"
	"go/token"
	"go/types"
)

func main() {
	exprs := []string{
		`1 + 2`,
		`"foo"`,
		`int{1,2,3}`, // error
		`[]int{1,2,3}`,
		`[3]int{1,2,3}`,
		`len([]int{1,2,3})`,
		`"foo" + 2`, // error
		`func(x, y int){ return x + y}`,
		`func(x, y int){ return x + y}(10,20)`,
	}

	fset := token.NewFileSet()

	for _, expr := range exprs {
		eval(fset, expr)
		fmt.Println("")
	}
}

func eval(fset *token.FileSet, expr string) {
	fmt.Println("input:", expr)
	tv, err := types.Eval(fset, nil, token.NoPos, expr)
	if err != nil {
		fmt.Println("error:", err)
		return
	}
	if tv.Type == nil {
		fmt.Printf("nil: %#v\n", tv)
		return
	}
	fmt.Printf("output: type=%#v\n", tv.Type)
	fmt.Printf("        value=%#v\n", tv.Value)
}
