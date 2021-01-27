package main

import (
	"fmt"
	"reflect"
)

type Person struct {
	Name string
}

func main() {
	p := Person{}
	rt := reflect.TypeOf(p)
	rv := reflect.ValueOf(p)

	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		rfv := rv.Field(i)
		if rfv.IsZero() {
			fmt.Println("hmm", rf.Name)
		}
	}

}
