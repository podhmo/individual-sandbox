package main

import (
	"fmt"
	"reflect"
)

type S struct {
	V *int
}

func main() {
	run(S{})
	fmt.Println("----------------------------------------")
	n := 10
	run(S{V: &n})
}
func run(v interface{}) {
	fmt.Printf("%[1]T: %+[1]v\n", v)
	switch v := v.(type) {
	case reflect.Value:
		switch v.Kind() {
		case reflect.Struct:
			for i := 0; i < v.NumField(); i++ {
				run(v.Field(i))
			}
		case reflect.Ptr:
			if v.IsNil() {
				return
			}
			run(v.Elem())
		case reflect.Int:
			fmt.Println(v.Int())
		default:
			panic(v)
		}
	default:
		run(reflect.ValueOf(v))
	}
}
