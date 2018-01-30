package main

import (
	"go/token"
	"go/types"

	"github.com/k0kubun/pp"
)

func main() {
	var tests = []string{
		`true`,
		`12345678 + 87654321 == 99999999`,
		`12345678 + 87654321`,
		`"aaa"`,
		`len([10]struct{}{})`,
		`len([10]struct{}{}) == 2*5`,
		`Person{}`,
	}
	fset := token.NewFileSet()
	for _, test := range tests {
		val, err := types.Eval(fset, nil, token.NoPos, test)
		pp.Println(val)
		if err != nil {
			pp.Println("!", err)
		}
	}
}
