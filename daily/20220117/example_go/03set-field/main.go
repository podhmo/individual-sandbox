package main

import (
	"fmt"
	"reflect"
	"unsafe"
)

type S struct {
	Name string
}

func main() {
	s := &S{Name: "foo"} // pointer is needed
	rv := reflect.ValueOf(s).Elem()

	rf := rv.FieldByName("Name")
	{
		rf := reflect.NewAt(rf.Type(), unsafe.Pointer(rf.UnsafeAddr())).Elem()
		fmt.Println(rf.Interface())
	}
	fmt.Println(rf.UnsafeAddr())
}
