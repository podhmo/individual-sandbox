package main

import (
	"fmt"
	"reflect"
	"text/template"
	_ "unsafe"
)

//go:linkname template_findFunction text/template.findFunction
func template_findFunction(name string, tmpl *template.Template) (reflect.Value, bool)

//go:linkname template_builtinFuncs text/template.builtinFuncs
func template_builtinFuncs() map[string]reflect.Value

func main() {
	for k, v := range template_builtinFuncs() {
		fmt.Printf("%s:	%[2]T	%+[2]v\n", k, v)
	}
	tmpl := template.New("tmpl")
	rfn, _ := template_findFunction("urlquery", tmpl)

	input := "foo?xxx=1&yyy=2"
	output := rfn.Call([]reflect.Value{reflect.ValueOf(input)})
	fmt.Println(output[0])
}
