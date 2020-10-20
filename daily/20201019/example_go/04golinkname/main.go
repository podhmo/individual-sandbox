package main

import (
	"fmt"
	"reflect"
	_ "text/template"
	_ "unsafe"
)

//go:linkname template_builtinFuncs text/template.builtinFuncs
func template_builtinFuncs() map[string]reflect.Value

func main() {
	bultins := template_builtinFuncs()
	for name, rv := range bultins {
		fmt.Printf("%10s: %[2]T %+[2]v\n", name, rv)
	}
}
