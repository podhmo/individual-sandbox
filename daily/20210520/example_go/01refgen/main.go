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
	v      int
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	var v W
	rv := reflect.ValueOf(&v).Elem() // need pointer
	rt := reflect.TypeOf(v)

	for i := 0; i < rt.NumField(); i++ {
		rfv := rv.Field(i)
		if !rfv.IsValid() || !rfv.CanSet() {
			fmt.Println("skip", i)
			continue
		}

		switch rfv.Kind() {
		case reflect.Ptr:
			if rfv.IsNil() {
				// strict only support, yet
				rfv.Set(reflect.New(rt.Field(i).Type.Elem())) // *Person (extract type only reflect.Type

			)
			}
		}
	}
	return nil
}
