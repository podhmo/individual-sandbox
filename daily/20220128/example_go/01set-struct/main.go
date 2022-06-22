package main

import (
	"fmt"
	"reflect"
)

type S struct {
	G *G
}
type G struct {
	Name string
}

func main() {
	s := &S{}
	rv := reflect.ValueOf(s).Elem()

	fv := rv.FieldByName("G")
	fmt.Printf("%#+v\n", s)

	// fv2 := reflect.NewAt(fv.Type().Elem(), unsafe.Pointer(fv.UnsafeAddr()))
	// fv.Set(fv2)
	z := reflect.Zero(fv.Type().Elem())
	n := reflect.New(fv.Type().Elem())
	fmt.Printf("z %#+v\n", z)
	fmt.Printf("n %#+v\n", n)
	fmt.Printf("n %#+v\n", n.Elem())
	fv.Set(n)
	fmt.Printf("%#+v\n", s)
	if s.G != nil {
		fmt.Println("ok", (*s.G).Name)
	}
}
