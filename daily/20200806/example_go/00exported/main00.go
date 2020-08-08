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
	fmt.Println(rv.FieldByName("Exported").Interface())
}
