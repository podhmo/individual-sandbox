package main

import (
	"fmt"
	"reflect"
)

type S struct {
	Exported   int
	unexported int
}

func main() {
	s := &S{Exported: 20, unexported: 10}
	rv := reflect.ValueOf(s).Elem()

	// panic: reflect.Value.Interface: cannot return value obtained from unexported field or method
	rf := rv.FieldByName("unexported")
	fmt.Println(rf.Interface())
}
