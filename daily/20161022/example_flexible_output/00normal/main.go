package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
	Info string `json:"-"`
}

func main() {
	person := &Person{Name: "foo", Age: 20, Info: "yay"}
	b, err := json.Marshal(person)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(b)) // {"name":"foo","age":20}
}
