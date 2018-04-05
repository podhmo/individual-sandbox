package main

import (
	"fmt"
	"go/types"
	"io"
	"os"

	"github.com/podhmo/tmp/indent"
)

func nillable(t types.Type) bool {
	U := types.Universe
	nilT := U.Lookup("nil").Type()
	return types.AssignableTo(nilT, t)
}

func writeCode(name string, t types.Type, w io.Writer) {
	o := indent.New(w)
	if nillable(t) {
		o.Printf("if %s != nil {\n", name)
		o.Indent()
		o.Printf("%s := *%s\n", name, name)
		defer func() {
			o.UnIndent()
			o.Println("}")
		}()
	}

	o.WithBlock(fmt.Sprintf("if p(%s)", name), func() {
		o.Printf("return %s\n", name)
	})
}

func writeCodeRec(name string, t types.Type, o *indent.Output) {
	if nillable(t) {
		o.WithBlock(fmt.Sprintf("if %s ! nil", name), func() {
			o.Println("x := *x")
			writeCodeRec(name, t.(*types.Pointer).Elem(), o)
		})
	} else {
		o.WithBlock(fmt.Sprintf("if p(%s)", name), func() {
			o.Printf("return %s\n", name)
		})

	}
}

func main() {
	U := types.Universe
	intT := U.Lookup("int").Type()

	{
		fmt.Println("int")
		writeCode("x", intT, os.Stdout)
	}

	fmt.Println("--")

	{
		fmt.Println("map[int]int")
		writeCode("c", types.NewMap(intT, intT), os.Stdout)
	}

	fmt.Println("--")
	{
		fmt.Println("*****int")
		p := types.NewPointer
		t := p(p(p(p(p(intT)))))
		writeCodeRec("x", t, indent.New(os.Stdout))
	}
}
