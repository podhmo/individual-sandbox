package main

import (
	"fmt"
	"reflect"

	"github.com/k0kubun/pp"
)

// []byte
// time.Time

func show(rt reflect.Type) {
	fmt.Println("@", rt, rt.Kind().String())
	pp.Println("@", rt)
}

func main() {
	show(reflect.TypeOf("foo"))
	show(reflect.TypeOf([]uint8{0}))
	show(reflect.TypeOf([]byte("foo")))
	fmt.Println(reflect.DeepEqual(reflect.TypeOf("foo"), reflect.TypeOf("foo")))
	fmt.Println(reflect.DeepEqual(reflect.TypeOf([]uint8{0}), reflect.TypeOf([]byte("foo"))))
}
