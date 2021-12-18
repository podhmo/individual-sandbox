package main

import (
	"encoding/json"
	"fmt"
	"os"
	"reflect"
)

type S struct {
	Name     string         `json:"name"`
	Age      int            `json:"age"`
	Stringer fmt.GoStringer `json:"stringer"`
}
type S2 struct {
	Name     string
	Age      int
	Stringer fmt.GoStringer
}

type Foo struct{ Name string }

func (f Foo) GoString() string {
	return "@" + f.Name + "@"
}

func main() {
	s := &S{Name: "foo", Age: 20, Stringer: Foo{Name: "X"}}
	json.NewEncoder(os.Stdout).Encode(s)
	{
		s := (*S2)(s)
		json.NewEncoder(os.Stdout).Encode(s)
	}

	// create new type
	rt := reflect.TypeOf(S{})
	var fields []reflect.StructField
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if !rf.IsExported() {
			continue
		}

		if rf.Type.Kind() == reflect.Interface {
			rf2 := rf
			rf2.Tag = reflect.StructTag(`json:"-"`)
			fields = append(fields, rf2)
		} else {
			fields = append(fields, rf)
		}
	}
	rt2 := reflect.StructOf(fields) // S3

	{
		rv := reflect.ValueOf(s)
		rv2 := rv.Convert(reflect.PtrTo(rt2))
		json.NewEncoder(os.Stdout).Encode(rv2.Interface())
	}
}
