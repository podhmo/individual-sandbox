package main

import (
	"encoding/json"
	"fmt"
)

// Person :
type Person struct {
	Name string
	Age  int
}

func main() {
	person := &Person{Name: "foo", Age: 20}
	b, err := json.Marshal(person)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(b))
}
