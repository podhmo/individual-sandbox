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

type WPerson struct {
	*Person
}

type I interface{}
type IPerson struct {
	I
}

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	p := Person{Name: "foo", Age: 20}

	fmt.Println("----------------------------------------")
	encoder.Encode(&p)

	fmt.Println("----------------------------------------")
	encoder.Encode(&WPerson{Person: &p})

	fmt.Println("----------------------------------------")
	encoder.Encode(&IPerson{I: &p})
}
