package main

import (
	"fmt"
	"reflect"
	"runtime"
	_ "unsafe"
)

type P struct{}

func (p *P) Foo() {
	fmt.Println("@@")
}

func main() {
	rt := reflect.TypeOf(&P{})
	rf, ok := rt.MethodByName("Foo")
	fmt.Println(rf, ok)

	fmt.Println(reflect.ValueOf(rf.Func.Interface()).Pointer())
	fmt.Println(rf.Func.Pointer())
	ptr := rf.Func.Pointer()

	rfunc := runtime.FuncForPC(ptr)
	fmt.Println(rfunc.Name())
}
