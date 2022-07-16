package main

import (
	"encoding/json"
	"fmt"
	"os"
	"reflect"
)

type Person struct {
	Name    string   `json:"name"`
	Friends []string `json:"friends"`
}

func main() {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "\t")

	foo := Person{Name: "foo", Friends: nil}
	enc.Encode(foo)

	fmt.Println("----------------------------------------")

	rv := reflect.ValueOf(&foo).Elem()
	rv.FieldByName("Name").SetString("**bar**")
	rv.FieldByName("Friends").Set(reflect.MakeSlice(reflect.TypeOf([]string{}), 0, 0))
	enc.Encode(foo)
}
