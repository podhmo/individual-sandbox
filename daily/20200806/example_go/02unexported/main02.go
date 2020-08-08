package main

import (
	"fmt"
	"reflect"
	"unsafe"
)

type S struct {
	Exported   int
	unexported int
}

func main() {
	s := &S{Exported: 20, unexported: 10}
	rv := reflect.ValueOf(s).Elem()

	rf := rv.FieldByName("unexported")
	rf = reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
	fmt.Println(rf.Interface())
}
