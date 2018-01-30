package main

import (
	"go/token"
	"go/types"

	"github.com/k0kubun/pp"
)

func main() {
	var tests = []string{
		`
struct Person {
	Name string
	Age int
}
`,
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
