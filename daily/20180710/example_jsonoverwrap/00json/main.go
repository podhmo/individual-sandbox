package main

import (
	"encoding/json"
	"log"
	"os"
)

type Meta struct {
	Memo string `json:"memo"`
}

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
	Meta Meta   `json:"meta"`
}

type WrapPerson struct {
	*Person
	Meta Meta `json:"-"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	encoder := json.NewEncoder(os.Stdout)

	p := Person{Name: "foo", Age: 20}
	if err := encoder.Encode(&p); err != nil {
		return err
	}
	if err := encoder.Encode(&WrapPerson{Person: &p}); err != nil {
		return err
	}
	return nil
}
