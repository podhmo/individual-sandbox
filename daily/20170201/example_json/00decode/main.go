package main

import (
	"encoding/json"
	"os"
)

// Person :
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// IPerson :
type IPerson interface{}

// PersonWithStruct :
type PersonWithStruct struct {
	Person
	Type string `json:"type"`
}

// PersonWithInterface :
type PersonWithInterface struct {
	IPerson
	Type string `json:"type"`
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	person := Person{Name: "foo", Age: 20}

	{
		wperson := PersonWithStruct{Type: "struct", Person: person}
		if err := encoder.Encode(wperson); err != nil {
			panic(err)
		}
	}
	{
		wperson := PersonWithInterface{Type: "interface", IPerson: person}
		if err := encoder.Encode(wperson); err != nil {
			panic(err)
		}
	}
}
