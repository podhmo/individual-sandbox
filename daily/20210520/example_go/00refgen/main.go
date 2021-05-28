package main

import (
	"fmt"
	"log"
	"reflect"
)

type Person struct {
	Name string
	Age  int
}

type W struct {
	Person *Person
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	var v *W // startがこれだとダメなのだった
	rv := reflect.ValueOf(v)
	rt := reflect.TypeOf(v)
	if rv.IsNil() {
		if rv.Kind() != reflect.Ptr || rv.IsNil() {
			return fmt.Errorf("invalid type %v", reflect.TypeOf(v))
		}

		rob := reflect.New(rt.Elem()).Elem()
		fmt.Printf("%#+v\n", rob.Interface())
		fmt.Println(rv.Pointer())
		// rv.Elem().Set(reflect.New(rt.Elem()).Elem())
		// fmt.Println("ok")
		// rv.Set(reflect.New(rt).Elem())
		fmt.Println("ok")
	}
	return nil
}
