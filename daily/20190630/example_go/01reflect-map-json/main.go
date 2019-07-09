package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

func main() {
	s := []byte(`{"x": "y"}`)
	var iface map[string]interface{}
	if err := json.Unmarshal(s, &iface); err != nil {
		panic(err)
	}
	rv := reflect.ValueOf(iface)
	fmt.Println(rv.MapIndex(reflect.ValueOf("x")).String())
	fmt.Println(rv.MapIndex(reflect.ValueOf("z")).String())
}
