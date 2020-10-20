package main

import (
	"fmt"
	"reflect"
	"runtime"
	"text/template"
	"unsafe"
)

//go:linkname template_findFunction text/template.findFunction
func template_findFunction(name string, tmpl *template.Template) (reflect.Value, bool)

func main() {
	tmpl := template.New("ROOT")
	rfn, ok := template_findFunction("urlquery", tmpl)
	if !ok {
		panic("not found")
	}

	s := "?xxx=111&yyy=222"
	rvs := rfn.Call([]reflect.Value{reflect.ValueOf(s)})
	fmt.Println(len(rvs), rvs[0].String())

	rfunc := runtime.FuncForPC(rfn.Pointer())
	fname, line := rfunc.FileLine(rfunc.Entry())
	fmt.Println(rfunc.Name(), fname, line)

	fn := *(*func(...interface{}) string)(unsafe.Pointer(rfn.Pointer()))
	fmt.Println("use unsafe", fn(s))
}
