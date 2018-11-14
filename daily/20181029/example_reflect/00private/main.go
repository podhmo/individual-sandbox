package main

import (
	"fmt"
	"reflect"

	"github.com/k0kubun/pp"
)

type P struct {
	x int
}

func main() {
	p := &P{}
	rv := reflect.ValueOf(p)
	pp.Println(reflect.Indirect(rv).FieldByName("x"))
	pp.Println(reflect.Indirect(rv).FieldByName("x").Type())
	fmt.Println(reflect.Indirect(rv).FieldByName("x").Type())
}
