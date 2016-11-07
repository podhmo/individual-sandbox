package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	b, err := json.MarshalIndent(Person{Name: "foo", Age: 20}, "", "  ")
	if err != nil {
		panic(err)
	}
	var ob interface{}
	var ob2 interface{}
	if err := json.Unmarshal(b, &ob); err != nil {
		panic(err)
	}
	if err := json.Unmarshal(b, &ob2); err != nil {
		panic(err)
	}
	fmt.Println(reflect.DeepEqual(ob, ob2))
}
