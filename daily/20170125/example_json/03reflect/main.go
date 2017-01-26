package main

import (
	"encoding/json"
	"fmt"
	"os"
	"reflect"
)

type Person struct {
	Name   string
	Age    int
	Father interface{}
	Mother interface{}
}

func Walk(ob interface{}) map[string]interface{} {
	rv := reflect.ValueOf(ob)
	// pointer
	if rv.Kind() == reflect.Ptr {
		rv = rv.Elem()
	}
	rt := rv.Type()
	m := make(map[string]interface{})
	for i := 0; i < rv.NumField(); i++ {
		switch name := rt.Field(i).Name; name {
		case "Father", "Mother":
			// noop
		default:
			m[name] = rv.Field(i).Interface()
		}
	}
	return m
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	p := Person{Name: "foo", Age: 20}

	fmt.Println("----------------------------------------")
	encoder.Encode(&p)

	fmt.Println("----------------------------------------")
	encoder.Encode(Walk(p))

	var i interface{}
	i = &p
	fmt.Println("----------------------------------------")
	encoder.Encode(Walk(i))
}
