package main

import (
	"encoding/json"
	"fmt"
	"os"
)

type Person struct {
	Name string
	Age  int
}

type Wrap struct {
	Age int `json:"-"`
	*Person
}

type Zero struct {
	Person    Person  `json:"person,omitempty"`
	PersonPtr *Person `json:"personPtr,omitempty"`
}

func main() {
	{
		w := Wrap{Person: &Person{Name: "foo", Age: 20}}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		fmt.Println(enc.Encode(w))
	}
	fmt.Println("----------------------------------------")
	{
		w := Zero{}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		fmt.Println(enc.Encode(w))
	}
}
